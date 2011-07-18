function mycsvwrite(filename,var,header)
% writes a CSV file with headers
% mycsvwrite(filename,var,header)
% header is a comma separeted list of names
% for example, if you have a 25x4 matrix M you can do:
%   mycsvwrite('datafile.csv',M,'id,wage,age,educ')

outid = fopen(filename, 'w+');
fprintf(outid, '%s\n', header);
for i = 1:size(var,1)
    outLine = regexprep(num2str(var(i,:)), '  *', ',');
    fprintf(outid, '%s\n', outLine);
end
fclose(outid);
