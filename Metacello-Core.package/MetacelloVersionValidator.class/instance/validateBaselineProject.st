private
validateBaselineProject
  | project |
  project := self
    validateProjectCreationFrom: self configurationClass
    onError: [ :ex | 
      self
        recordValidationError:
          'Error creating project: ' , ex description
            , ' to reproduce evalutate the following: '''
            , self configurationClass name asString , ' project'''
        callSite: #'validateBaselineProject'
        reasonCode: #'projectCreationError'.
      ^ self validationReport ].
  ^ self validateBaselineProject: project