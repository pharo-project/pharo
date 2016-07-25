method generation
methodSource
  | strm |
  strm := WriteStream on: String new.
  self methodSelectorAndPragma: self selector imports: self imports on: strm.
  self
    methodSection: self
    pre: [ :methodSection :indent | 
      strm
        cr;
        tab: indent;
        nextPutAll: 'spec for: ' , methodSection attributePrintString , ' do: [';
        cr.
      methodSection versionSpec
        configMethodOn: strm
        last: methodSection methodSections isEmpty
        indent: indent + 1 ]
    last: false
    post: [ :methodSection :indent :last | 
      strm nextPutAll: ' ].'.
      (last
        or: [ indent = 1 or: [ methodSection methodSections isEmpty and: [ indent = 1 ] ] ])
        ifTrue: [ strm cr ] ]
    indent: 0.
  ^ strm contents