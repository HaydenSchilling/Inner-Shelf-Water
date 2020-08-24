
% solitary canyon

if (1==0)

close all
clear all

nc =  'E:\AAUNSWharddrives\bkhome1stMay07\matlab\SS\seasor\procCTD\SS-2004-08_020.nc';

T = nc_varget(nc,'Temp');
S = nc_varget(nc,'Salt');
sigma = nc_varget(nc,'Sigma');
lat = nc_varget(nc,'Lat');
lon = nc_varget(nc,'Lon');
pres = nc_varget(nc,'Press');
for ii = 2:length(lat)
 xdist(ii) = sw_dist([lat(1) lat(ii)],[lon(1) lon(ii)],'km');
end
clf
pcolor(xdist,pres,T');shading flat;colorbar;hold on;set(gca,'clim',[10 22]);
colormap(jet(24));
[c,s] = contour(xdist,pres,S',34:0.1:36,'w');clabel(c,s,'Color','w');
set(s(:),'Linewidth',2);
set(gca,'ydir','r');
xlabel('Distance along transect [km]');
ylabel('depth [m]');
title('Temperature and salinity in Solitary Canyon');

print -dpng E:\work\SS200408\solitary_canyon_SS200408.png

%  now reproduce opcbins

end

close all
clear all
file = 'E:\AAUNSWharddrives\bkhome1stMay07\matlab\SS\seasor\opc\SS-2004-08_020.csv';ttitle = 'Solitary Canyon';tran = 2;secc = 2;

xx = 3; % odd number

bins = [0, 106, 208, 312, 420, 531, 645, 761, 880, 1001, 1123, 1247,...
        1373, 1501, 1630, 1760, 1891, 2024, 2158, 2292, 2248, 2565, 2703,...
        2842, 2981, 3122, 3263]/1e6; % m

limits = [0, 75, 142, 249, 357, 468, 582 698, 816, 936, 1058, 1182,...
          1307, 1434, 1562, 1692, 1823, 1955, 2088, 2223, 2358, 2495,...
          2632, 2770, 2910, 3050, 3191, 3332, 100000]/1e6; % m

sample = 5:19;
nobin = size(sample,2);
          
w = 4*pi/3*((bins(sample)/2).^3);
dw = 4*pi/3*((limits(sample+1)/2).^3)-4*pi/3*((limits(sample)/2).^3);
  
data = csvread(file,2,1);

% abundance: sum over xx data points

for i=floor(xx/2)+1:size(data,1)-floor(xx/2)
    for bb = sample(1):sample(end)
     N(i,bb-sample(1)+1) = sum(data(i-floor(xx/2):i+floor(xx/2),8+bb));
    end
end

% turn a distance into a velocity:

mdist = diff(data(:,7))/2;

for i = 3:size(mdist,1)-2
mdist(i) = mean(mdist(i-1:i+1));
end

mdist = [mdist;mdist(end)];

for i=floor(xx/2)+1:size(data,1)-floor(xx/2)
    % get biomass across all size-classes (xx*2 seconds; 0.002 m2 aperture;ship speed 4 m s-1)
    b(i) = sum(N(i,1:nobin).*w)./(2*mdist(i)*xx*0.002); % m^{-3}
end

for i = floor(xx/2)+1:size(data,1)-floor(xx/2)
    
% remove all points above the first zero bin
    maxn(i) = min([find(N(i,1:size(w,2))==0) size(w,2)]);
    accpts = 1:maxn(i)-1;
    % alternatively, just remove all zero bins.
    % accpts = find(N(i,1:size(w,2))~=0);
   [P,S] = polyfit(log(w(accpts)),log(N(i,accpts).*w(accpts)./dw(accpts)),1);
   sslope(i) = P(1); iint(i)= P(2);
end
clear P S i bb

time = data(floor(xx/2)+1:size(data,1)-floor(xx/2),1);

lat = data(floor(xx/2)+1:size(data,1)-floor(xx/2),2);
lon = data(floor(xx/2)+1:size(data,1)-floor(xx/2),3);
depth = data(floor(xx/2)+1:size(data,1)-floor(xx/2),5);
slope = sslope(floor(xx/2)+1:size(data,1)-floor(xx/2))';
inttt = iint(floor(xx/2)+1:size(data,1)-floor(xx/2))';
temp = data(floor(xx/2)+1:size(data,1)-floor(xx/2),6);
bbb = b(floor(xx/2)+1:size(data,1)-floor(xx/2));
bot = data(floor(xx/2)+1:size(data,1)-floor(xx/2),4);
salt = data(floor(xx/2)+1:size(data,1)-floor(xx/2),6);

% remove data that causes a problem

good = find(lat~=0);

bot(find(bot==0))=NaN

dist = [0;cumsum(sw_dist(lat(good),lon(good),'km'))];


range_depth = max(depth(good)) - min(depth(good));
range_dist = max(dist)-min(dist);
scatter(lon,depth,10,bbb*1e9,'filled');set(gca,'ydir','r','xlim',[153.45 153.66]);set(gca,'clim',[0 400]);colorbar;colormap(jet(20));title('Biovolume [mm-1 m-3]')

break


ff = range_depth/range_dist;

[disti,depthi] = meshgrid(min(dist):range_dist/50:max(dist),min(depth(good)):range_depth/50:max(depth(good)));
tempi = griddata(dist*ff,depth(good),data(good,6),disti*ff,depthi);
bi = griddata(dist*ff,depth(good),b(good),disti*ff,depthi);
slopei = griddata(dist*ff,depth(good),sslope(good),disti*ff,depthi);
countsi = griddata(dist*ff,depth(good),sum(N(good),2),disti*ff,depthi);

clf

masterres=12;

colormap(1-gray(masterres));

h = axes('Position',[0.1 0.35 0.80 0.23])

set(gca,'FontSize',12);
contourf(disti,depthi,bi,0:6e-6/masterres:6e-6);set(gca,'CLIM',[0 6e-6]);

initpos = get(gca,'Position');
handcol = colorbar;

hold on
[c1,h1] = contour(disti,depthi,tempi,16:0.5:20,'-k');
clabel(c1,h1,'FontWeight','bold');
set(h1,'LineWidth',1,'LineStyle','-');
hold on
plot(dist,depth(good),'.k','MarkerSize',2);
hold on
title(['Biovolume in greyscale (m^{3}m^{-3}) and Temperature (^oC) contours on ',ttitle]);
plot(dist,bot(good),'-k','LineWidth',1);
set(gca,'ydir','reverse');
ylabel('depth (m)');
set(gca,'XTickLabel','');

if (tran==2)
text(48,2,'B','FontSize',16,'Fontweight','bold');
else
text(-1,2,'B','FontSize',16,'Fontweight','bold');
end
set(gca,'Position',initpos);
set(handcol,'Position',[0.92 initpos(2)+0.05 0.03 initpos(4)-0.06])

h = axes('Position',[0.1 0.65 0.80 0.23])

set(gca,'FontSize',12);

% calculate sigma_t

if (tran < 10)

eval(['pdens = sw_pden(SS0408_00',num2str(tran),'.Salinity,SS0408_00',num2str(tran),'.Temperature,[SS0408_00',num2str(tran),'.Pressure*ones(1,size(SS0408_00',num2str(tran),'.Salinity,1))]'',1);'])

% ok, so property-property plots, griddata  salt

eval(['ttdist = cumsum([-0.5 sw_dist(SS0408_00',num2str(tran),'.Lat,SS0408_00',num2str(tran),'.Lon'',''km'')''])']);
eval(['ttdepth = SS0408_00',num2str(tran),'.Pressure;']);
eval(['ttsalt = SS0408_00',num2str(tran),'.Salinity;']);

else

eval(['pdens = sw_pden(SS0408_0',num2str(tran),'.Salinity,SS0408_0',num2str(tran),'.Temperature,[SS0408_0',num2str(tran),'.Pressure*ones(1,size(SS0408_0',num2str(tran),'.Salinity,1))]'',1);'])

% ok, so property-property plots, griddata  salt

eval(['ttdist = cumsum([-0.5 sw_dist(SS0408_0',num2str(tran),'.Lat,SS0408_0',num2str(tran),'.Lon'',''km'')''])']);
eval(['ttdepth = SS0408_0',num2str(tran),'.Pressure;']);
eval(['ttsalt = SS0408_0',num2str(tran),'.Salinity;']);


end

salti = griddata(ttdist*ff,ttdepth,ttsalt',disti*ff,depthi);

[c11,h11] = contourf(disti,depthi,salti,35.4:0.025:35.7,'k');

%eval(['[c11,h11] = contourf(cumsum([-0.5 sw_dist(SS0408_00',num2str(tran),'.Lat,SS0408_00',num2str(tran),'.Lon'',''km'')'']),SS0408_00',num2str(tran),'.Pressure,SS0408_00',num2str(tran),'.Salinity'',35.4:0.025:35.7,''k'');']);

initpos = get(gca,'Position');
handcol = colorbar('vert');

set(gca,'ydir','reverse');
ylabel('depth (m)');

if (file == '/c/mbaird/home/matlab/SS/seasor/opc/SS-2004-08_002.csv')
 set(gca,'xdir','reverse');
end

set(gca,'clim',[35.4 35.7]);
set(gca,'XTicklabel','');

if (tran==2)
text(48,2,'A','FontSize',16,'Fontweight','bold');
else
text(-1,2,'A','FontSize',16,'Fontweight','bold');
end

hold on

if tran < 10

eval(['[c13,h13] = contour(cumsum([-0.5 sw_dist(SS0408_00',num2str(tran),'.Lat,SS0408_00',num2str(tran),'.Lon'',''km'')'']),SS0408_00',num2str(tran),'.Pressure,(pdens-1000)'',25.0:0.1:26.2,''w'');']);

else

eval(['[c13,h13] = contour(cumsum([-0.5 sw_dist(SS0408_0',num2str(tran),'.Lat,SS0408_0',num2str(tran),'.Lon'',''km'')'']),SS0408_0',num2str(tran),'.Pressure,(pdens-1000)'',25.0:0.1:26.2,''w'');']);

end


clabel(c13,h13,'FontWeight','bold','Color','w');
set(h13,'LineWidth',1,'LineStyle','--');
title(['Salinity in greyscale (PSU) and \sigma contours (kg m^{-3}) on ',ttitle]);

set(gca,'Position',initpos);
set(handcol,'Position',[0.92 initpos(2)+0.04 0.03 initpos(4)-0.05])

%eval(['print -deps2 biomass',num2str(tran),'.eps']);
%eval(['print -djpeg90 biomass',num2str(tran),'.jpg']);

% now put in pareto

h = axes('Position',[0.1 0.1 0.785 0.19])

colormap(1-gray(masterres));
sspareto;
set(gca,'Position',initpos);
set(hhh,'Position',[0.92 initpos(2) 0.03 initpos(4)]);

if (tran==2)
text(48,2,'C','FontSize',16,'Fontweight','bold');
else
text(-1,2,'C','FontSize',16,'Fontweight','bold');
end

%h = axes('Position',[0.1 0.10 0.85 0.38])

%ghg = find(bi<0.5e-6);

%slopei(ghg) = -1.5;

%set(gca,'FontSize',12);
%contourf(disti,depthi,slopei,-1.25:1.0/masterres:-0.5);set(gca,'CLIM',[-1.25 -0.5]);hhh = colorbar;
%shading flat
%hold on
%[c1,h1] = contour(disti,depthi,tempi,16:0.5:20,'-k');
%clabel(c1,h1,'FontWeight','bold');
%set(h1,'LineWidth',1,'LineStyle','-');
%set(hhh,'Ytick',[-1.21 -1.0:0.25:-0.5]);
%set(hhh,'YtickLabel',['Low  ';'-1.0 ';'-0.75';'-0.5 ']);

%eval(['[c11,h11] = contour(cumsum([-0.5 sw_dist(SS0408_00',num2str(tran),'.Lat,SS0408_00',num2str(tran),'.Lon'',''km'')'']),SS0408_00',num2str(tran),'.Pressure,SS0408_00',num2str(tran),'.Salinity'',35.4:0.05:35.7,''-w'');']);
%clabel(c11,h11,'FontWeight','bold','Color',[1 1 1]);

%hold on
%plot(dist,depth(good),'.k','MarkerSize',2);
%plot(dist,bot(good),'-k','LineWidth',1);
%set(gca,'ydir','reverse');
%if (file == '/c/mbaird/home/matlab/SS/seasor/opc/SS-2004-08_002.csv')
% set(gca,'xdir','reverse');
% set(gca,'XTick',0:5:45);
% set(gca,'XTickLabel',['45';'40';'35';'30';'25';'20';'15';'10';' 5';' 0']);
%end
%ylabel('depth (m)');
%xlabel('Distance along transect (km)');
%title(['Slope of the NBSS on ',ttitle]);

brighten(0.4);

orient tall

eval(['print -deps2 seasoar',num2str(tran),'.eps']);
