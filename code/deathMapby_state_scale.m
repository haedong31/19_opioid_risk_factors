% Author: Haedong Kim
% Created Date: 2019-09-23

clear; close all; clc;

%% KFF data importing options 
opts = delimitedTextImportOptions("NumVariables", 7);

% Specify range and delimiter
opts.DataLines = [4, 55];
opts.Delimiter = ",";

% Specify column names and types
opts.VariableNames = ["Location", "age_0_24", "age_25_34", "age_35_44",...
    "age_45_54", "age_over55", "Total"];
opts.VariableTypes = ["string", "double", "double", "double", "double",...
    "double", "double"];
opts = setvaropts(opts, 1, "WhitespaceRule", "preserve");
opts = setvaropts(opts, 1, "EmptyFieldRule", "auto");
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

%% Draw maps
% get file paths of KFF Opioid number of deaths data by year
file_paths = dir('./data/kff/*.csv');

% get the maximum number of deaths for colormap
kff_data_maxs = zeros(1, numel(file_paths));
kff_data_mins = zeros(1, numel(file_paths));
for i = 1:numel(file_paths)
    % import data
    path_i = sprintf('./data/kff/%s', file_paths(i).name);
    kff_data = readtable(path_i, opts);
    
    % remove the first row that is for whole U.S.
    kff_data(1, :) = [];
    
    % find the maximum number of deaths for each year
    kff_data_maxs(i) = max(kff_data.Total);
    kff_data_mins(i) = min(kff_data.Total);
end

% structure a map object
figure
ax = usamap('all');
set(ax, 'Visible', 'off')
us_map = shaperead('usastatehi', 'UseGeoCoords', true);
names = {us_map.Name};
indexHawaii = strcmp('Hawaii',names);
indexAlaska = strcmp('Alaska',names);
indexConus = 1:numel(us_map);
indexConus(indexHawaii|indexAlaska) = []; 

for i = 1:numel(file_paths)
    % import data
    path_i = sprintf('./data/kff/%s', file_paths(i).name);
    kff_data = readtable(path_i, opts);
    
    % assign code number to NaN's
    kff_data.Total(isnan(kff_data.Total)) = -99;
    
    % states and year of the imported data
    states = kff_data.Location;
    strs1 = strsplit(file_paths(i).name, '_');
    strs2 = strsplit(string(strs1(4)), '.');
    year = strs2(1);
    
    % assign number of deaths for each state in 'us_map'
    for j = 1:numel(us_map)
        idx = find(states == us_map(j).Name);
        us_map(j).Deaths = kff_data.Total(idx);
    end
    
    % figure features
    caxis([0 kff_data_maxs(i)])
    colormap(jet)
    colorbar
    txt = ['Opioid Overdose Deaths in the U.S.' year];
    title(txt);
    
    % remove frames from the figure
    for j = 1:3
        setm(ax(j), 'Frame', 'off', 'Grid', 'off', 'ParallelLabel',...
            'off', 'MeridianLabel', 'off');
    end
    
    % draw a map
    faceColors = makesymbolspec('Polygon', ...
        {'Deaths', [kff_data_mins(i) kff_data_maxs(i)], 'FaceColor', jet}, ...
        {'Deaths', -99, 'FaceColor', 'black'});
    
    hold on; 
    geoshow(ax(1), us_map(indexConus), 'DisplayType', 'polygon',...
        'SymbolSpec', faceColors);
    geoshow(ax(2), us_map(indexAlaska), 'DisplayType', 'polygon',...
        'SymbolSpec', faceColors);
    geoshow(ax(3), us_map(indexHawaii), 'DisplayType', 'polygon',...
        'SymbolSpec', faceColors);
    F(i) = getframe(gcf);
    hold off;
end

%% Video recording
video_obj = VideoWriter('deaths_state_rel.mp4', 'MPEG-4');
video_obj.FrameRate = 1;
open(video_obj);
writeVideo(video_obj, F);
close(video_obj);
