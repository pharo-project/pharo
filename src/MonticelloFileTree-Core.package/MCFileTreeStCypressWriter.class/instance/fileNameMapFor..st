private
fileNameMapFor: aMethodDefinitionCollection
  "https://github.com/dalehenrich/filetree/issues/92"

  "answer a dictionary that maps each definition selector to a filename that is guaranteed unique on case insensitive file systems.
  Segregate instance and class side methods. Key is true for class method map, false for instance method map"

  | map filenameMetaMap |
  map := Dictionary new.
  aMethodDefinitionCollection
    do: [ :mDef | 
      | sel col metaKey methMap |
      "sort into bins by lowercase selector. "
      metaKey := mDef classIsMeta.
      methMap := map
        at: metaKey
        ifAbsent: [ map at: metaKey put: Dictionary new ].
      sel := mDef selector asLowercase.
      col := methMap
        at: sel
        ifAbsent: [ methMap at: sel put: OrderedCollection new ].
      col add: mDef ].
  filenameMetaMap := Dictionary new.
  map
    keysAndValuesDo: [ :metaKey :methMap | 
      | filenameMap |
      filenameMap := filenameMetaMap
        at: metaKey
        ifAbsent: [ filenameMetaMap at: metaKey put: Dictionary new ].
      methMap values
        do: [ :col | 
          | selector sortedCol |
          col size = 1
            ifTrue: [ 
              | def |
              "no need to distinguish filename"
              def := col at: 1.
              filenameMap
                at: def selector
                put: (self fileNameForSelector: def selector) ]
            ifFalse: [ 
              "tack on postfix to guarantee  file names are uniique on case insensitive file systems"
              sortedCol := col sorted: [ :a :b | a asString <= b asString ].
              (1 to: sortedCol size) do: [ :index | 
                | def filename |
                def := sortedCol at: index.
                filename := self fileNameForSelector: def selector.
                filename := filename , '..' , index printString.
                filenameMap at: def selector put: filename ] ] ] ].
  ^ filenameMetaMap