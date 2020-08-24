
close all

%load ctd_data
%load hydro_data

load E:\AAUNSWharddrives\bkhome1stMay07\matlab\SS\patrick\matfiles\ctd_data.mat
load E:\AAUNSWharddrives\bklocalDec2007\matlab\hydro_data.mat

clear min

% fill extra point in HS1.castO2 and HS4.castO2

HS1.castO2(4,2) = mean([HS1.castO2(3,2) HS1.castO2(5,2)]);
HS4.castO2(6,2) = mean([HS4.castO2(5,2) HS4.castO2(7,2)]);
HS4.castO2(8,3) = mean([HS4.castO2(7,3) HS4.castO2(9,3)]);

HS6.castO2(2,2) = mean([HS6.castO2(2,1) HS6.castO2(2,3)]);
HS6.castO2(2,end) = mean([HS6.castO2(4,end) HS6.castO2(2,end)]);


% get rid of zero depths.

HS5.depth(HS5.depth==0) == NaN;
HS7.depth(HS7.depth==0) == NaN;
HS8.depth(HS8.depth==0) == NaN;

HS7.phosphate(2,end) = 0;

masterres = 20;

flres = 40:25/masterres:65;
chlres = (flres-54.742)/2.7807;
o2res = 160:100/masterres:260;
o2satres = 60:50/masterres:110;
nres = 0:20/masterres:20;
pres = 0:2/masterres:2;
sres = 0:20/masterres:20;
saltres = 34:2/masterres:36.0;
tempres = 1:20/masterres:21;

Transect_name=[
'153 E           ';
'153 30 E        ';
'153 E           ';
'Sydney          ';
'Diamond Head    ';
'N Solitary (30S)';
'Wooli (29 40S)  ';
'Evans Head (29S)'];

depth = [300 300 300 300 125 300 100 200];

for ii=6:6

  figure

  colormap(0.9-[0:1/masterres:1]'*ones(1,3).*0.9); 
  colormap(jet(length(o2res)-1)); 
   
  eval(['xdiff=nanmean(diff(Section0' num2str(ii) '.longitude));']);
  eval(['ydiff=nanmean(diff(Section0' num2str(ii) '.latitude));']);

  if abs(xdiff)>abs(ydiff)

    eval(['x0=nanmean(Section0' num2str(ii) '.longitude);']);
    eval(['y0=nanmean(Section0' num2str(ii) '.latitude);']);    

    eval(['lon=Section0' num2str(ii) '.longitude;']);
    eval(['lat=Section0' num2str(ii) '.latitude;']);
    
    xdist=[0 cumsum(sw_dist(lat,lon,'km'))];
    %xdist=xdist-mean(xdist);
    
    if (lon(end)<lon(1))
      
      xdist=-xdist;
    
    end;
    
  else
    
    eval(['x0=nanmean(Section0' num2str(ii) '.longitude);']);
    eval(['y0=nanmean(Section0' num2str(ii) '.latitude);']);    

    eval(['lon=Section0' num2str(ii) '.longitude;']);
    eval(['lat=Section0' num2str(ii) '.latitude;']);
    
    xdist=[0 cumsum(sw_dist(lat,lon,'km'))];
    xdist=xdist-mean(xdist);
    
    if (lat(end)<lat(1))
    
      xdist=-xdist;
      
    end;
    
  end;
  
  rows = 3;
  cols = 2;
  vspc=0.07;
  hspc=0.02;
  left=0.1;
  right = 0.1;
  bot=0.2;
  top = 0.05;
  wdth= (1.0 - left - right-(cols-1)*hspc)/cols;
  hgt= (1.0 - bot -top -(rows-1)*vspc)/rows;
    
        col = 1;row =1;
        h = axes('position',[left+(col-1)*(wdth+hspc) (1.0-top-row*(hgt+vspc)) wdth hgt]);
        
        eval(['plot(xdist,Section0' num2str(ii) '.bottom,''k'',''linewidth'',2);']);
        hold on
        eval(['[ch1,h1]=contourf(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.flourescence,flres);']);
        %eval(['[ch1,h1]=contourf(xdist,Section0' num2str(ii) '.pressure,(Section0' num2str(ii) '.flourescence-54.742)/2.7807,chlres);']);
        oldpos = get(gca,'Position');
        cb = colorbar('vert');
        cbpos = get(cb,'Position');
        set(gca,'Position',[oldpos(1) oldpos(2) oldpos(3)*0.88 oldpos(4)]);
        set(cb,'Position',[oldpos(1) + oldpos(3)*0.88 + 0.01 cbpos(2)+0.03 0.02 cbpos(4)-0.06]); 
        set(gca,'clim',[flres(1) flres(end)]);
        %set(gca,'clim',[chlres(1) chlres(end)]);
        %eval(['[ch1,h1]=contour(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.salinity,35.4:0.05:35.7,''-w'',''LineWidth'',0.5);']);
        set(gca,'ydir','reverse','ylim',[0 depth(ii)],'ytick',0:50:300,'xlim',[min(xdist) max(xdist)],'xtick',[-15:5:35]);
        %clabel(ch1,h1,35.4:0.1:35.7,'color','w');
        ylabel('Pressure (dbar)');
        set(gca,'Xticklabel','');
        text(mean(xdist),-40*depth(ii)/300,'Fluorescence (dimensionless)','HorizontalAlignment','center');
        %text(0,-40*depth(ii)/300,'Chl a (mg m^{-3})','HorizontalAlignment','center');
        plot(xdist,ones(size(xdist)),'v','Color','k','MarkerFaceColor','auto','MarkerSize',6,'MarkerFaceColor','k');
        if (ii==4)
          set(gca,'xdir','reverse');
          text(max(xdist)+(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'A','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        else
          text(min(xdist)-(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'A','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        end
        col = 2;row =1;
        h = axes('position',[left+(col-1)*(wdth+hspc) (1.0-top-row*(hgt+vspc)) wdth hgt]);

        eval(['plot(xdist,Section0' num2str(ii) '.bottom,''k'',''linewidth'',2);']);
        hold on
        if (ii<5)
           eval(['[ch1,h1]=contourf([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.nitrate,nres);']);
        else
            eval(['[ch1]=pcolor([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.nitrate);']);
            shading interp
          set(gca,'clim',[nres(1) nres(end)]);
        end
        oldpos = get(gca,'Position');
        cb = colorbar('vert');
        cbpos = get(cb,'Position');
        set(gca,'Position',[oldpos(1) oldpos(2) oldpos(3)*0.88 oldpos(4)]);
        set(cb,'Position',[oldpos(1) + oldpos(3)*0.88 + 0.01 cbpos(2)+0.03 0.02 cbpos(4)-0.06]); 
        set(gca,'clim',[nres(1) nres(end)]);

        %eval(['[ch1,h1]=contour(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.salinity,35.4:0.05:35.7,''-w'',''LineWidth'',0.5);']);
        set(gca,'ydir','reverse','ylim',[0 depth(ii)],'ytick',0:50:300,'xlim',[min(xdist) max(xdist)],'xtick',[-15:5:35]);
        %clabel(ch1,h1,35.4:0.1:35.7,'color','w');
        eval(['plot([xdist''*ones(size(HS',num2str(ii),'.depth,1),1)'']'',HS',num2str(ii),'.depth,''.w'');']);
        set(gca,'Yticklabel','');
        set(gca,'Xticklabel','');
        text(mean(xdist),-40*depth(ii)/300,'Nitrate (mmol m^{-3})','HorizontalAlignment','center');
        if (ii==4)
          set(gca,'xdir','reverse');
          text(max(xdist)+(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'B','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        else
          text(min(xdist)-(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'B','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        end
        col = 1;row =2;
        h = axes('position',[left+(col-1)*(wdth+hspc) (1.0-top-row*(hgt+vspc)) wdth hgt]);

% calculate O2 characteristics

eval(['dens = sw_dens(Section0',num2str(ii),'.temperature,Section0',num2str(ii),'.salinity,Section0',num2str(ii),'.pressure''*ones(1,size(Section0',num2str(ii),'.temperature,2)))']);

        eval(['pdens = sw_pden(Section0',num2str(ii),'.salinity,Section0',num2str(ii),'.temperature,Section0',num2str(ii),'.pressure''*ones(1,size(Section0',num2str(ii),'.temperature,2)),1025*ones(size(Section0',num2str(ii),'.temperature)))']);

        eval(['sw02 = sw_satO2(Section0',num2str(ii),'.salinity,Section0',num2str(ii),'.temperature).*44.61']);
  % eval(['sw02 = sw_satO2(Section0',num2str(ii),'.salinity,Section0',num2str(ii),'.temperature).*44.6628./(1.0 + pdens/1000).*(1+dens/1000)']);

        eval(['plot(xdist,Section0' num2str(ii) '.bottom,''k'',''linewidth'',2);']);
        hold on
        eval(['[ch1,h1]=contourf(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.DO.*100./sw02,o2satres);']);
        
        oldpos = get(gca,'Position');
        cb = colorbar('vert');
        cbpos = get(cb,'Position');
        set(gca,'Position',[oldpos(1) oldpos(2) oldpos(3)*0.88 oldpos(4)]);
        set(cb,'Position',[oldpos(1) + oldpos(3)*0.88 + 0.01 cbpos(2)+0.03 0.02 cbpos(4)-0.06]); 
        set(gca,'clim',[o2satres(1) o2satres(end)]);
        %eval(['[ch1,h1]=contour(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.salinity,35.4:0.05:35.7,''-w'',''LineWidth'',0.5);']);
        eval(['[ch1,h1]=contour([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.castO2,o2res,''-w'',''LineWidth'',0.5);']);
        set(gca,'ydir','reverse','ylim',[0 depth(ii)],'ytick',0:50:300,'xlim',[min(xdist) max(xdist)],'xtick',[-15:5:35]);
        eval(['plot([xdist''*ones(size(HS',num2str(ii),'.depth,1),1)'']'',HS',num2str(ii),'.depth,''.w'');']);
        clabel(ch1,h1,160:10:260,'color','w');
        hold on;
        ylabel('Pressure (dbar)');
        set(gca,'Xticklabel','');
        text(mean(xdist),-40*depth(ii)/300,'O_2: CTD (% saturation) and Bottle (mmol m^{-3})','HorizontalAlignment','center');
        plot(xdist,ones(size(xdist)),'v','Color','k','MarkerFaceColor','auto','MarkerSize',6,'MarkerFaceColor','k');
        if (ii==4)
          set(gca,'xdir','reverse');
          text(max(xdist)+(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'C','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        else
          text(min(xdist)-(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'C','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        end
        col = 2;row =2;
        h = axes('position',[left+(col-1)*(wdth+hspc) (1.0-top-row*(hgt+vspc)) wdth hgt]);

        eval(['plot(xdist,Section0' num2str(ii) '.bottom,''k'',''linewidth'',2);']);
        hold on
        if (ii<5)
           eval(['[ch1,h1]=contourf([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.phosphate,pres);']);
        
        else
            eval(['[ch1]=pcolor([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.phosphate);']);
            shading interp
        set(gca,'clim',[pres(1) pres(end)]);
        end
        oldpos = get(gca,'Position');
        cb = colorbar('vert');
        cbpos = get(cb,'Position');
        set(gca,'Position',[oldpos(1) oldpos(2) oldpos(3)*0.88 oldpos(4)]);
        set(cb,'Position',[oldpos(1) + oldpos(3)*0.88 + 0.01 cbpos(2)+0.03 0.02 cbpos(4)-0.06]); 
        set(gca,'clim',[pres(1) pres(end)]);
        hold on
        %eval(['[ch1,h1]=contour(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.salinity,35.4:0.05:35.7,''-w'',''LineWidth'',0.5);']);
        set(gca,'ydir','reverse','ylim',[0 depth(ii)],'ytick',0:50:300,'xlim',[min(xdist) max(xdist)],'xtick',[-15:5:35]);
        %clabel(ch1,h1,35.4:0.1:35.7,'color','w');
        eval(['plot([xdist''*ones(size(HS',num2str(ii),'.depth,1),1)'']'',HS',num2str(ii),'.depth,''.w'');']);
        set(gca,'Yticklabel','');
        set(gca,'Xticklabel','');
        text(mean(xdist),-40*depth(ii)/300,'Phosphate (mmol m^{-3})','HorizontalAlignment','center');
        if (ii==4)
          set(gca,'xdir','reverse');
          text(max(xdist)+(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'D','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        else
          text(min(xdist)-(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'D','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        end
        col = 1;row =3;
        h = axes('position',[left+(col-1)*(wdth+hspc) (1.0-top-row*(hgt+vspc)) wdth hgt]);

        o2res = 160:10:260;

        eval(['plot(xdist,Section0' num2str(ii) '.bottom,''k'',''linewidth'',2);']);
        hold on

        %if (ii<5)
        %   eval(['[ch1,h1]=contourf([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.castO2,o2res);']);
        %else
        %    eval(['[ch1]=pcolor([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.castO2);']);
        %    shading interp
        % set(gca,'clim',[o2res(1) o2res(end)]);
        %end

        eval(['[ch1,h1]=contourf(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.salinity,saltres);']);
        oldpos = get(gca,'Position');
        cb = colorbar('vert');
        cbpos = get(cb,'Position');
        set(gca,'Position',[oldpos(1) oldpos(2) oldpos(3)*0.88 oldpos(4)]);
        set(cb,'Position',[oldpos(1) + oldpos(3)*0.88 + 0.01 cbpos(2)+0.03 0.02 cbpos(4)-0.06]); 
        set(gca,'clim',[saltres(1) saltres(end)]);
        eval(['[ch1,h1]=contour(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.temperature,tempres,''-w'',''LineWidth'',0.5);']);
        set(gca,'ydir','reverse','ylim',[0 depth(ii)],'ytick',0:50:300,'xlim',[min(xdist) max(xdist)],'xtick',[-15:5:35]);
        clabel(ch1,h1,tempres,'color','w');
        ylabel('Pressure (dbar)');
        text(mean(xdist),360*depth(ii)/300,'distance [km]','HorizontalAlignment','center');
        %text(0,-40*depth(ii)/300,'Bottle Oxygen (mmol m^{-3})','HorizontalAlignment','center');
        text(mean(xdist),-40*depth(ii)/300,'Salinity (PSU) and Temperature (^oC)','HorizontalAlignment','center');
        plot(xdist,ones(size(xdist)),'v','Color','k','MarkerFaceColor','auto','MarkerSize',6,'MarkerFaceColor','k');
        if (ii==4)
          set(gca,'xdir','reverse');
          text(max(xdist)+(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'E','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        else
          text(min(xdist)-(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'E','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        end
        col = 2;row =3;
        h = axes('position',[left+(col-1)*(wdth+hspc) (1.0-top-row*(hgt+vspc)) wdth hgt]);

        eval(['plot(xdist,Section0' num2str(ii) '.bottom,''k'',''linewidth'',2);']);
        hold on
        if (ii<5)
           eval(['[ch1,h1]=contourf([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.silicate,sres);']);
        else
            eval(['[ch1]=pcolor([xdist''*ones(10,1)'']'',HS',num2str(ii),'.depth,HS',num2str(ii),'.silicate);']);
            shading interp
            set(gca,'clim',[sres(1) sres(end)]);
        end
        oldpos = get(gca,'Position');
        cb = colorbar('vert');
        pos = get(gca,'Position');
        cbpos = get(cb,'Position');
        set(gca,'Position',[oldpos(1) oldpos(2) oldpos(3)*0.88 oldpos(4)]);
        set(cb,'Position',[oldpos(1) + oldpos(3)*0.88 + 0.01 cbpos(2)+0.03 0.02 cbpos(4)-0.06]); 
        set(gca,'clim',[sres(1) sres(end)]);
        eval(['[ch1,h1]=contour(xdist,Section0' num2str(ii) '.pressure,Section0' num2str(ii) '.sigma_t,25:0.25:27,''-w'',''LineWidth'',0.5);']);
        set(gca,'ydir','reverse','ylim',[0 depth(ii)],'ytick',0:50:300,'xlim',[min(xdist) max(xdist)],'xtick',[-15:5:35]);
        clabel(ch1,h1,25:0.5:27,'color','w');
        eval(['plot([xdist''*ones(size(HS',num2str(ii),'.depth,1),1)'']'',HS',num2str(ii),'.depth,''.w'');']);
        set(gca,'Yticklabel','');
        text(mean(xdist),360*depth(ii)/300,'distance [km]','HorizontalAlignment','center');
        text(mean(xdist),-40*depth(ii)/300,'Silicate (mmol m^{-3}) and \sigma_t','HorizontalAlignment','center');
        if (ii==4)
          set(gca,'xdir','reverse');
          text(max(xdist)+(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'F','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        else
          text(min(xdist)-(max(xdist)-min(xdist))*0.1,-50*depth(ii)/300,'F','HorizontalAlignment','center','Color',[0 0 0],'FontSize',16,'FontWeight','bold');
        end
 %  eval(['supertitle([deblank(Transect_name(ii,:)) '' '' Section0' num2str(ii) '.date(1,:) '' '' Section0' num2str(ii) '.time(1,:) '' - '' Section0' num2str(ii) '.date(end,:) '' '' Section0' num2str(ii) '.time(end,:) '' UTC''])']);
   eval(['print -depsc2 transect',num2str(ii),'.eps']);
   eval(['print -djpeg90 transect',num2str(ii),'.jpg']);
   
  end;
  
print -dpng E:\work\SS200408\ss200408ctds.png