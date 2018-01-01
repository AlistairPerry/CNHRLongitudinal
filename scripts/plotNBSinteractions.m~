function plotNBSinteractions(nbsfile, connectivitymats, grptimeinfo, ages, grplabels, baseoutputstring)
% Construct plots to guide interpretation of significant group x time
% interactions derived from the NBS
% USAGE plotNBSinteractions(nbsfile, connectivitymats, grptimeinfo, ages, grplabels)

%Inputs:
%1. _nbsfile_: Filename of saved NBS output structure

%And pre-loaded within matlab workspace
%2. _Connectivitymats_: Connectivity matrices of all subjects (_N_) concatenated within a 3D matrix
%3. _grptimeinfo_: _N_ x 2 structure representing group (1st column) and
%time (2nd column) information of each individual (row)
% * where, first group is indexed by _1_, and the second group _-1_
% * and, time one (baseline) denoted by _1_ and time two (follow up) as _-1_

%4. _age_: Structure representing age of individuals at both baseline and
%followup
% * NOTE: Ordering of subjects group (and time) plus age information stored in vectors must match that stored within
% _connectivitymats_

%5. _grplabels_: Group labels identified within cell structure for plotting
% * For example, for control (CN) and high-risk (HR) groups, {'CN','HR'}

%6. _baseoutputstring_

%load nbs structure
load(nbsfile);

%identify rows referring to different time and groups
GP1ind=find(grptimeinfo(:,1)==1);
GP2ind=find(grptimeinfo(:,1)==-1);

T1=find(grptimeinfo(:,2)==1);
T2=find(grptimeinfo(:,2)==-1);

%find groups at different times
GP1T1=intersect(T1,GP1ind);
GP1T2=intersect(T2,GP1ind);

GP2T1=intersect(T1,GP2ind);
GP2T2=intersect(T2,GP2ind);

%extract ages for line fitting later on
GP1T2ages=ages(GP1T2,:);
GP1T1ages=ages(GP1T1,:);
GP2T2ages=ages(GP2T2,:);
GP2T1ages=ages(GP2T1,:);

%identify group time label strings for plots
GP1T1label=[grplabels{1,1} ' ' 'Base'];
GP1T2label=[grplabels{1,1} ' ' 'FU'];

GP2T1label=[grplabels{1,2} ' ' 'Base'];
GP2T2label=[grplabels{1,2} ' ' 'FU'];

%plots for each network identified within particular NBS threshold
for i = 1:length(nbs.NBS.con_mat(1,:))
    
    %extract subject connectivity weights
    %as performed in extractNBSinformation.m
    
    NBSweights=[];
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    for k = 1:length(xcol)
        NBSweights(:,k)=squeeze(connectivitymats(xcol(k,1),ycol(k,1),:));
    end
    
    NBSweightssum(:,i)=sum(NBSweights,2);
    
    %now group connectivity weights at each time point
    
    GP1T1net=[];
    GP2T1net=[];
    GP1T2net=[];
    GP2T2net=[];
    
    GP1T1net=NBSweightssum(GP1T1,i);
    GP2T1net=NBSweightssum(GP2T1,i);
    GP1T2net=NBSweightssum(GP1T2,i);
    GP2T2net=NBSweightssum(GP2T2,i);
    
    %basic bar and sem plots
    
    meanNBSgrpweights(i,:)=[mean(GP1T1net) mean(GP1T2net) mean(GP2T1net) mean(GP2T2net)];
    grpweightssem(i,:)=[std(GP1T1net)./sqrt(length(GP1T1net)) std(GP1T2net)./sqrt(length(GP1T2net)) std(GP2T1net)./sqrt(length(GP2T1net)) std(GP2T1net)./sqrt(length(GP2T2net))];
    
    %group one
    figure
    hold on
    
    bar([1;2],[meanNBSgrpweights(i,1); meanNBSgrpweights(i,2)],'FaceColor','k')
    
    hold on
    
    %group two
    bar([3;4],[meanNBSgrpweights(i,3); meanNBSgrpweights(i,4)],'FaceColor','r')
    errorbar(meanNBSgrpweights(i,:), grpweightssem(i,:),'k','LineStyle','none','LineWidth',1)
    
    set(gca,'XTick',[1 2 3 4])
    set(gca,'box','off')
    set(gca,'XTickLabel',{GP1T1label, GP1T2label, GP2T1label, GP2T2label})
    
    ylabel('Mean Streamline Weights', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Group and timepoint', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    %ax.XTickLabel=[GP1T1label, GP1T2label, GP2T1label, GP2T2label];
    
    hold off
    
    %distribution plots for each group and time point
    
    weightscell={GP1T1net,GP1T2net,GP2T1net,GP2T2net};
    figure,plotSpread_incmarkersz(weightscell,'distributionColors',{'k','k','r','r'})
    
    set(gca,'XTick',[1 2 3 4])
    set(gca,'box','off')
    set(gca,'XTickLabel',{GP1T1label, GP1T2label, GP2T1label, GP2T2label})
    
    ylabel('Streamline Weights', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Group and timepoint', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    hold off
    
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
    
    ylabel('Streamline Weights', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Age', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    hold off
    
    %estimate and fit quadratics for each group at different points
    
    agerg=min(ages):0.1:max(ages);
    
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
    
    ylabel('Streamline Weights', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    xlabel('Age', 'FontSize', 14, 'FontWeight', 'Bold', 'Color', 'black')
    
    ax=gca;
    ax.YColor = 'black';
    ax.XColor = 'black';
    ax.FontWeight = 'bold';
    ax.FontSize = 12;
    
    legend('show',GP1T1label,GP1T2label,GP2T1label,GP2T2label,'Location','SouthWest','boxoff')
    
    hold off
end
end