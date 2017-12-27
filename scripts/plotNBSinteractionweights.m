function plotNBSinteractionweights(nbsfile, connectivitymats, grptimeinfo, ages)

load(nbsfile);

    %identify rows referring to different time and groups
    CNind=find(grptimeinfo(:,1)==1);
    HRind=find(grptimeinfo(:,1)==-1);
    
    T1=find(grptimeinfo(:,2)==1);
    T2=find(grptimeinfo(:,2)==-1);
    
    %find groups at different times
    CN1=intersect(T1,CNind);
    CN2=intersect(T2,CNind);
    
    HR1=intersect(T1,HRind);
    HR2=intersect(T2,HRind);
    
    %extract ages
    CN1ages=ages(CN1,:);
    HR1ages=ages(HR1,:);
    CN2ages=ages(CN2,:);
    HR2ages=ages(HR2,:);
    
    %CNages(:,2)=CNages(:,1)+2;
    %HRages(:,2)=HRages(:,1)+2;

for i = 1:length(nbs.NBS.con_mat(1,:))
    
    %extract subject connectivity weights

    NBSweights=[];
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    for k = 1:length(xcol)
        NBSweights(:,k)=squeeze(connectivitymats(xcol(k,1),ycol(k,1),:));
    end
    
    NBSweightssum(:,i)=sum(NBSweights,2);
    
    %now group weights
    
    CNnet1=[];
    HRnet1=[];
    CNnet2=[];
    HRnet2=[];
    
    CNnet1=NBSweightssum(CN1,i);
    HRnet1=NBSweightssum(HR1,i);
    CNnet2=NBSweightssum(CN2,i);
    HRnet2=NBSweightssum(HR2,i);
    
    %basic bar plots
    
    meanNBSgrpweights(i,:)=[mean(CNnet1) mean(CNnet2) mean(HRnet1) mean(HRnet2)];
    grpweightssem(i,:)=[std(CNnet1)./sqrt(length(CNnet1)) std(CNnet2)./sqrt(length(CNnet2)) std(HRnet1)./sqrt(length(HRnet1)) std(HRnet1)./sqrt(length(HRnet2))];

    figure
    bar([1;2],[meanNBSgrpweights(i,1); meanNBSgrpweights(i,2)],'FaceColor','k')
    hold on
    bar([3;4],[meanNBSgrpweights(i,3); meanNBSgrpweights(i,4)],'FaceColor','r')
    errorbar(meanNBSgrpweights(i,:), grpweightssem(i,:),'k','LineStyle','none','LineWidth',1)
    
    hold off
    
    %distribution plots
    weightscell={CNnet1,CNnet2,HRnet1,HRnet2};
    hold on
    figure,plotSpread_incmarkersz(weightscell,'distributionColors',{'k','k','r','r'})
    hold off
    %dim=1;
    
    %dmhr=[];
    %dmhr(:,1)=ones(length(HR1),1); dmhr(:,2)=dim+ones(length(HR1),1);
    
    %dmcn=[];
    %dmcn(:,1)=ones(length(CN1),1); dmcn(:,2)=dim+ones(length(CN1),1);
    
    %now plot
    
    figure
    hold on
    
    %first CN's   
    
    for j = 1:length(HR1)
        plot([HR1ages(j,1) HR2ages(j,1)], [HRnet1(j,1) HRnet2(j,1)],'-r','LineWidth',1,'Marker','o','MarkerFaceColor','r','MarkerEdgeColor','k','MarkerSize',6)
    end
    
    for j = 1:length(CN1)
        plot([CN1ages(j,1) CN2ages(j,1)], [CNnet1(j,1) CNnet2(j,1)],'-k','LineWidth',1,'Marker','o','MarkerFaceColor','k','MarkerEdgeColor','k','MarkerSize',6)
    end
    
    %plot(dmhr,HRnet,'-r.','markersize',18),
    %hold on, plot(dmcn,CNnet,'-k.','markersize',18),
    %xlim([0.75 2.25])
    
    hold off
    
    %estimate and fit quadratics
    
    agerg=min(ages):0.1:max(ages);
    
    p_CN1=polyfit(CN1ages,CNnet1,2);
    y_CN1=polyval(p_CN1,agerg);
    figure, plot(agerg,y_CN1,'k','linewidth',3)
    p_CN2=polyfit(CN2ages,CNnet2,2);
    y_CN2=polyval(p_CN2,agerg);
    hold on
    plot(agerg,y_CN2,'k--','linewidth',3)
    p_HR1=polyfit(HR1ages,HRnet1,2);
    y_HR1=polyval(p_HR1,agerg);
    plot(agerg,y_HR1,'r','linewidth',3)
    p_HR2=polyfit(HR2ages,HRnet2,2);
    y_HR2=polyval(p_HR2,agerg);
    plot(agerg,y_HR2,'r--','linewidth',3)
    
    hold off
end

end