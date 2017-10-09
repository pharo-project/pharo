extraction
extractMethodSectionsForClass: aBaselineClass
  | pragma |
  pragma := self extractBaselinePragmaFor: aBaselineClass.
  self evaluatePragma: pragma.
  self methodSections
    do: [ :methodSection | self evaluateBaselineMethodSection: methodSection ]