private
normalizeTagsData: jsonObject
  "return a dictionay mapping the tag name to the commit SHA"

  | tagDict |
  jsonObject isDictionary
    ifTrue: [ 
      jsonObject
        at: 'message'
        ifPresent: [ :errorString | 
          self
            error:
              'Error accessing tags for github project: ' , self projectPath printString
                , ' -> ' , errorString printString ] ].
  tagDict := Dictionary new.
  jsonObject
    do: [ :tagObject | 
      | tag commit |
      tag := tagObject at: 'name'.
      commit := tagObject at: 'commit'.
      tagDict at: tag put: (commit at: 'sha') ].
  ^ tagDict