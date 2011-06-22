function mycsvwrite(filename,var,header)

outid = fopen(filename, 'w+');
fprintf(outid, '%s\n', header);
for i = 1:size(var,1)
    outLine = regexprep(num2str(var(i,:)), '  *', ',');
    fprintf(outid, '%s\n', outLine);
end
fclose(outid);