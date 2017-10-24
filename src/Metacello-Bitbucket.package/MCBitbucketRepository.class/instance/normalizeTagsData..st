private
normalizeTagsData: jsonObject
  "return a dictionay mapping the tag name to the commit SHA"

  | tagDict |
  jsonObject
    at: 'error'
    ifPresent: [ :errorObject | 
      self
        error:
          'Error accessing tags for bitbucket project: ' , self projectPath printString
            , ' -> ' , (errorObject at: 'message') printString
            ,
              ((errorObject at: 'detail')
                ifNil: [ '' ]
                ifNotNil: [ :detail | ' :: ' , detail printString ]) ].
  tagDict := Dictionary new.
  jsonObject
    keysAndValuesDo: [ :tag :tagObject | 
      | sha |
      sha := tagObject at: 'node'.
      tagDict at: tag put: sha ].
  ^ tagDict