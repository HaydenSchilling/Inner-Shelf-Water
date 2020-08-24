% Calibration of Fl read from screen on cruise versus Chl a.

Fl = [39 37 13 54 63 93 104 18 27 30 33 19 24 33 18 26 28 33 56 62 40 72 78];

Chl = [1.3590425
1.21585
0.5718425
1.5171025
1.926405
1.9881475
2.1498575
0.822945
1.06137
1.564785
1.0204725
1.3818275
0.58839125
1.128423333
0.7805025
0.5479125
0.30752375
0.2520275
0.991435
1.4254225
0.38643
1.25026125
1.60385];

[p,s] = polyfit(Chl,Fl',1);

hh = plot(Chl,Fl,'+k',0:0.1:3,p(1)*[0:0.1:3]+p(2),'-k');
set(gca,'FontSize',14);
title('Chl:Fl calibration curve');
xlabel('Laboratory Measured Chl a');
ylabel('In Situ Observed Fl.');
lh = legend(hh,['Samples'],...
        ['Fl = ',num2str(p(1)),'*[Chl a] + ',num2str(p(2))],'Location','NorthWest')
