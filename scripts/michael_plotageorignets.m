M=dataforageplots2;
M=table2array(M);
%Find rows for T1 + T2 and CN + AR
 
clear T1 T1 AR* CN*
 
T1=find(M(:,2)==1);
T2=find(M(:,2)==2);
CN=find(M(:,1)==1);
AR=find(M(:,1)==2);
AR1=intersect(T1,AR);
AR2=intersect(T2,AR);
CN1=intersect(T1,CN);
CN2=intersect(T2,CN);
 
CNages(1,:)=M(CN1,3)';
CNages(2,:)=M(CN2,3)';
ARages(1,:)=M(AR1,3)';
ARages(2,:)=M(AR2,3)';
 
 
%Populate AR + CN net 1 + 2
ARnet1(1,:)=M(AR1,4)';
ARnet1(2,:)=M(AR2,4)';
ARnet2(1,:)=M(AR1,5)';
ARnet2(2,:)=M(AR2,5)';
ARnet3(1,:)=M(AR1,6)';
ARnet3(2,:)=M(AR2,6)';
 
CNnet1(1,:)=M(CN1,4)';
CNnet1(2,:)=M(CN2,4)';
CNnet2(1,:)=M(CN1,5)';
CNnet2(2,:)=M(CN2,5)';
CNnet3(1,:)=M(CN1,6)';
CNnet3(2,:)=M(CN2,6)';
 
%% basic 3d scatter plots
dim=1;
 
dmar=[];
dmar(1,:)=ones(1,61); dmar(2,:)=dim*ones(1,61);
 
dmcn=[];
dmcn(1,:)=ones(1,73); dmcn(2,:)=dim*ones(1,73);
 
figure (11), clf,
plot3(dmar,ARages,ARnet1,'-r.','markersize',18),
hold on, plot3(dmcn,CNages,CNnet1,'-k.','markersize',18),
xlim([0 3])
 
figure (12), clf,
plot3(dmar,ARages,ARnet2,'-r.','markersize',18),
hold on, plot3(dmcn,CNages,CNnet2,'-k.','markersize',18),
xlim([0 3])
 
figure (13), clf,
plot3(dmar,ARages,ARnet3,'-r.','markersize',18),
hold on, plot3(dmcn,CNages,CNnet3,'-k.','markersize',18),
xlim([0 3])
 
%% estimate and fit qudratics
 
ages=15:0.1:34; 
 
%net 1
p_AR1N1=polyfit(ARages(1,:),ARnet1(1,:),2);
y_AR1N1=polyval(p_AR1N1,ages);
figure(11), plot3(ones(size(ages)),ages,y_AR1N1,'r','linewidth',3)
 
p_CN1N1=polyfit(CNages(1,:),CNnet1(1,:),2);
y_CN1N1=polyval(p_CN1N1,ages);
hold on, plot3(ones(size(ages)),ages,y_CN1N1,'k','linewidth',3)
 
p_AR2N1=polyfit(ARages(2,:),ARnet1(2,:),2);
y_AR2N1=polyval(p_AR2N1,ages);
hold on, plot3(dim.*ones(size(ages)),ages,y_AR2N1,'r:','linewidth',3)
 
p_CN2N1=polyfit(CNages(2,:),CNnet1(2,:),2);
y_CN2N1=polyval(p_CN2N1,ages);
hold on, plot3(dim.*ones(size(ages)),ages,y_CN2N1,'k:','linewidth',3)
 
if dim==1; view([90 0]), axis tight, end
%net 2
 
p_AR1N2=polyfit(ARages(1,:),ARnet2(1,:),2);
y_AR1N2=polyval(p_AR1N2,ages);
figure(12), plot3(ones(size(ages)),ages,y_AR1N2,'r','linewidth',3)