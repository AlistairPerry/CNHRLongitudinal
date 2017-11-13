function [NBSweights]=extractNBSinformation(nbsfile, connectivitymatrices, AALstrings, COG, basefilename)

load(nbsfile);
nbstab = [];
nbscell = cell(1,2);

parc=length(nbs.NBS.test_stat);
numsubjs=size(connectivitymatrices,3);

%extract subject connectivity weights

for i = 1:length(nbs.NBS.con_mat(1,:))
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    
    for j = 1:numsubjs
        cons=[];
        for k = 1:length(xcol)
            cons(k,1)=connectivitymatrices(xcol(i,1),ycol(i,1),j);
        end
        NBSweights(j,i) = sum(cons);
    end
    
end

dlmwrite([basefilename '' 'subjweights.txt'], NBSweights, 'delimiter', '\t');

%now extract output tables

for i = 1:length(nbs.NBS.con_mat(1,:))
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    for k = 1:length(xcol(:,1))
        if isempty(nbstab)
            nbstab(1,1) = i;
            nbstab(1,2) = xcol(k,1);
            nbstab(1,3) = ycol(k,1);
            nbscell(1,1) = AALstrings(xcol(k,1), 1);
            nbscell(1,2) = AALstrings(ycol(k,1), 1);
        else
            nbstab(numel(nbstab(:,1))+1, 1) = i;
            nbstab(numel(nbstab(:,2)), 2) = xcol(k,1);
            nbstab(numel(nbstab(:,3)), 3) = ycol(k,1);
            nbscell(numel(nbscell(:,1))+1,1) = {AALstrings{xcol(k,1), 1}};
            nbscell(numel(nbscell(:,2)), 2) = {AALstrings{ycol(k,1), 1}};
        end
    end
end

fid = fopen([basefilename '' 'results.txt'], 'wt');
for i = 1:length(nbstab(:,1))
    fprintf(fid, '%d\t%d\t%d\t%s\t%s\n', nbstab(i,1), nbstab(i,2), nbstab(i,3), nbscell{i,1}, nbscell{i,2});
end
fclose(fid)


%Create output for BNV

%subnetworks
%nodes
for i = 1:length(nbs.NBS.con_mat(1,:))
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    nodes = cat(1,xcol,ycol);
    nodes = unique(nodes);
    
    fid = fopen([basefilename '' int2str(i) '' '.node'], 'wt');
    for j = 1:length(nodes)
        fprintf(fid, '%f\t%f\t%f\t%d\t%d\t%s\n', COG(nodes(j,1),1), COG(nodes(j,1),2), COG(nodes(j,1),3), 1, 1, '-');
    end
end
fclose(fid)

%edges
fkmtx = zeros(length(xcol),length(xcol));
for k = 1:length(xcol)
    [iloc,~] = find(xcol(k,1)==nodes);
    [jloc,~] = find(ycol(k,1)==nodes);
    fkmtx(iloc,jloc) = 1;
    fkmtx(jloc,iloc) = 1;
end
dlmwrite([basefilename '' int2str(i) '' '.edge'], fkmtx, 'delimiter', '\t', 'newline', 'pc');

end