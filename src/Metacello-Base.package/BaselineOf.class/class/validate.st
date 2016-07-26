accessing
validate
  "Check the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). 
	Errors identify specification issues that will result in unexpected behaviour when you load the configuration. 
	Critical Warnings identify specification issues that may result in unexpected behavior when you load the configuration.
	Warnings identify specification issues that are technically correct, but are worth take a look at."

  "self validate"

  <apiDocumentation>
  self ensureMetacello.
  ^ ((Smalltalk at: #'MetacelloToolBox')
    validateBaseline: self
    debug: #()
    recurse: false) explore