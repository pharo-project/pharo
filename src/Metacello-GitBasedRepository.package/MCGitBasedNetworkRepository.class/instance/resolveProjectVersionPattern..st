private
resolveProjectVersionPattern: aString
  "aString must conform to the syntax for MetacelloVersionNumber with the exception that aString may have a leading $v which is stripped before conversion to a MetacelloVersionNumber patterm"

  | patternString tagDictionary matchingTags tagAssocs sortedMatchingTags theTag |
  self projectPath
    ifNil: [ self error: 'projectPath must be set to resolve project version pattern.' ].
  patternString := (self class projectVersionFromString: aString) asString.
  tagDictionary := self downloadJSONTags.
  tagAssocs := tagDictionary keys
    collect: [ :tagName | 
      | tagVersion |
      tagVersion := self class projectVersionFromString: tagName.
      tagVersion -> tagName ].
  matchingTags := tagAssocs select: [ :assoc | assoc key match: patternString ].
  matchingTags isEmpty
    ifTrue: [ 
      projectVersion := aString.
      ^ self
        error:
          'No tags matching the pattern ' , aString printString
            , ' found for repository description '
            , self description printString ].
  sortedMatchingTags := matchingTags asArray sort: [ :a :b | a key <= b key ].
  theTag := sortedMatchingTags last.
  projectVersionPattern := aString.
  projectVersion := theTag value