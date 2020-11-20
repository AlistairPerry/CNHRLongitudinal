function NBSBNVext(nbsfile, basefname, COG, ParcNetIndex)

load(nbsfile);


FigOutDIR = '/Users/alistairp/Documents/Brisbane/CNHRLongitudinal_dropbox/NBS_Time/Plots';


%Create output for BNV

%subnetworks
%nodes

for i = 1:length(nbs.NBS.con_mat(1,:))
    [xcol, ycol] = find(nbs.NBS.con_mat{1,i});
    nodes = cat(1,xcol,ycol);
    nodes = unique(nodes);
    
    fid = fopen([FigOutDIR '/' basefname '_N' int2str(i) '' '.node'], 'wt');
    for j = 1:length(nodes)
        fprintf(fid, '%f\t%f\t%f\t%d\t%d\t%s\n', COG(nodes(j,1),1), COG(nodes(j,1),2), COG(nodes(j,1),3), ParcNetIndex(nodes(j,1),1), 1, '-');
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
dlmwrite([FigOutDIR '/' basefname '_N' int2str(i) '' '.edge'], fkmtx, 'delimiter', '\t', 'newline', 'pc');

end
