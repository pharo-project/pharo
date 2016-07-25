ChangeSets keep track of the changes made to a system, so they can be written on a file as source code (a "fileOut"). 

---
preamble and postscript:  two strings that serve as prefix (useful for documentation) and suffix (useful for doits) to the fileout of the changeSet.


changeRecords -  Dictionary {class name -> a ClassChangeRecord}.
These classChangeRecords (qv) remember all of the system changes.