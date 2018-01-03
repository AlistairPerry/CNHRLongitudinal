function plottopologyinteractions(ConTable, grplabels, baseoutputstring)
% Construct plots to guide interpretation of significant group x time
% interactions derived from topology measures
% USAGE plotNBSinteractions(ConTable, grplabels, baseoutputstring)

%Inputs:
%1. ConTable: Extracted table corresponding to topology metrics
%2. grplabels: Group labels identified within cell structure for plotting
% * For example, for control (CN) and high-risk (HR) groups, {'CN','HR'}
%3. baseoutputstring

%identify rows referring to different time and groups
GP1ind=find(ConTable.Group==1);
GP2ind=find(ConTable.Group==-1);

T1=find(ConTable.Time==1);
T2=find(ConTable.Time==-1);

%find groups at different times
GP1T1=intersect(T1,GP1ind);
GP1T2=intersect(T2,GP1ind);

GP2T1=intersect(T1,GP2ind);
GP2T2=intersect(T2,GP2ind);

%extract ages for line fitting later on
GP1T2ages=ConTable.Age(GP1T2,:);
GP1T1ages=ConTable.Age(GP1T1,:);
GP2T2ages=ConTable.Age(GP2T2,:);
GP2T1ages=ConTable.Age(GP2T1,:);

%identify group time label strings for plots
GP1T1label=[grplabels{1,1} ' ' 'Base'];
GP1T2label=[grplabels{1,1} ' ' 'FU'];

GP2T1label=[grplabels{1,2} ' ' 'Base'];
GP2T2label=[grplabels{1,2} ' ' 'FU'];

%plots for each network identified within particular NBS threshold
for i = 3:6
    
    TopLabel=char(ConTable.Properties.VariableNames(i));
    %now group topology weights at each time point
    
    GP1T1net=[];
    GP2T1net=[];
    GP1T2net=[];
    GP2T2net=[];
    
    GP1T1net=table2array(ConTable(GP1T1,i));
    GP2T1net=table2array(ConTable(GP2T1,i));
    GP1T2net=table2array(ConTable(GP1T2,i));
    GP2T2net=table2array(ConTable(GP2T2,i));
    
    %basic bar and sem plots
    
    meangrpweights(i,:)=[mean(GP1T1net) mean(GP1T2net) mean(GP2T1net) mean(GP2T2net)];
    grpweightssem(i,:)=[std(GP1T1net)./sqrt(length(GP1T1net)) std(GP1T2net)./sqrt(length(GP1T2net)) std(GP2T1net)./sqrt(length(GP2T1net)) std(GP2T1net)./sqrt(length(GP2T2net))];
    
    %group one
    figure
    hold on
    
    bar([1;2],[meangrpweights(i,1); meangrpweights(i,2)],'FaceColor','k')
    
    hold on
    
    %group two
    bar([3;4],[meangrpweights(i,3); meangrpweights(i,4)],'FaceColor','r')
    errorbar(meangrpweights(i,:), grpweightssem(i,:),'k','LineStyle','none','LineWidth',1)
    
    set(gca,'XTick',[1 2 3 4])
    set(gca,'box','off')
    set(gca,'XTickLabel',{GP1T1label, GP1T2label, GP2T1label, GP2T2label})
    
    ylabel(TopLabel, 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Group and timepoint', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    ax.YLim=[min(table2array(ConTable(:,i))) max(table2array(ConTable(:,i)))];
    
    %ax.XTickLabel=[GP1T1label, GP1T2label, GP2T1label, GP2T2label];
    
    savefig([baseoutputstring '_' TopLabel '_basicstats.fig']);
    saveas(gcf, [baseoutputstring '_' TopLabel '_basicstats.tif'],'tiffn');
    
    hold off
    
    %distribution plots for each group and time point
    
    weightscell={GP1T1net,GP1T2net,GP2T1net,GP2T2net};
    figure,plotSpread_incmarkersz(weightscell,'distributionColors',{'k','k','r','r'})
    
    set(gca,'XTick',[1 2 3 4])
    set(gca,'box','off')
    set(gca,'XTickLabel',{GP1T1label, GP1T2label, GP2T1label, GP2T2label})
    
    ylabel(TopLabel, 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Group and timepoint', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    hold off
    
    savefig([baseoutputstring '_' TopLabel '_distplots.fig']);
    saveas(gcf, [baseoutputstring '_' TopLabel '_distplots.tif'],'tiffn');
    
    figure
    hold on
    
    %plot individuals across each group and their connectivity trajectories
    %first group one
    
    for j = 1:length(GP1T1)
        plot([GP1T1ages(j,1) GP1T2ages(j,1)], [GP1T1net(j,1) GP1T2net(j,1)],'-k','LineWidth',1,'Marker','o','MarkerFaceColor','k','MarkerEdgeColor','k','MarkerSize',6)
    end
    
    %now group two
    
    for j = 1:length(GP2T1)
        plot([GP2T1ages(j,1) GP2T2ages(j,1)], [GP2T1net(j,1) GP2T2net(j,1)],'-r','LineWidth',1,'Marker','o','MarkerFaceColor','r','MarkerEdgeColor','k','MarkerSize',6)
    end
    
    set(gca,'box','off')
    
    ylabel(TopLabel, 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Age', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    savefig([baseoutputstring '_' TopLabel '_agetrajectories.fig']);
    saveas(gcf, [baseoutputstring '_' TopLabel '_agetrajectories.tif'],'tiffn');
        
    hold off
    
    %estimate and fit quadratics for each group at different points
    
    agerg=min(ConTable.Age):0.1:max(ConTable.Age);
    
    %Group 1, Time 1
    p_GP1T1=polyfit(GP1T1ages,GP1T1net,2);
    y_GP1T1=polyval(p_GP1T1,agerg);
    figure, plot(agerg,y_GP1T1,'k','linewidth',3)
    
    %Group 1, Time 2
    p_GP1T2=polyfit(GP1T2ages,GP1T2net,2);
    y_GP1T2=polyval(p_GP1T2,agerg);
    hold on
    plot(agerg,y_GP1T2,'k--','linewidth',3)
    
    %Group 2, Time 1
    p_GP2T1=polyfit(GP2T1ages,GP2T1net,2);
    y_GP2T1=polyval(p_GP2T1,agerg);
    plot(agerg,y_GP2T1,'r','linewidth',3)
    
    %Group 2, Time 2
    p_GP2T2=polyfit(GP2T2ages,GP2T2net,2);
    y_GP2T2=polyval(p_GP2T2,agerg);
    plot(agerg,y_GP2T2,'r--','linewidth',3)
    
    set(gca,'box','off')
    
    ylabel(TopLabel, 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Age', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    legend('show',GP1T1label,GP1T2label,GP2T1label,GP2T2label,'Location','SouthWest','boxoff')
    
    savefig([baseoutputstring '_' TopLabel '_agequadratics.fig']);
    saveas(gcf, [baseoutputstring '_' TopLabel '_agequadratics.tif'],'tiffn');
    
    hold off
end
end

