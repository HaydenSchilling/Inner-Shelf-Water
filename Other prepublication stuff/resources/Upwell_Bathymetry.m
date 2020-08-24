clear
close all

% Extract the bathymetry along each line of latitude
% Jason Everett (UQ/UNSW)
% 28th November 2019

% tbl = readtable('SS2004_SeaSoarData.csv');
% tbl.Lat2 = floor(tbl.Lat *10)/10;
% unique(tbl.Lat2)

Lat = [-31.8; -30.0; -29.0; -28.6];
Lon = 152:0.01:155;

[Lon2, Lat2] = meshgrid(Lon, Lat);

for a = 1:4
    Depth(a,:) = GEBCO_GetDepth(Lat2(a,:)',Lon2(a,:)');
end

tbl = table([NaN Lon; [Lat Depth]]);
writetable(tbl, 'Upwelling_Bathymetry_Data.csv', 'WriteVariableNames', false)