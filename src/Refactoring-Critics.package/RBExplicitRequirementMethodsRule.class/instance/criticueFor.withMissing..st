as yet unclassified
criticueFor: aClass withMissing: aSelector
    ^ (ReMissingMethodCritique for: aClass by: self class: aClass selector: aSelector) beShouldBeImplemented