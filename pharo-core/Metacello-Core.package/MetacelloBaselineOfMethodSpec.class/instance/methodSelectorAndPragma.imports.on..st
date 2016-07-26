method generation
methodSelectorAndPragma: aSelector imports: importList on: strm
  strm
    nextPutAll: aSelector asString , ' spec';
    cr;
    tab;
    nextPutAll: '<baseline>'