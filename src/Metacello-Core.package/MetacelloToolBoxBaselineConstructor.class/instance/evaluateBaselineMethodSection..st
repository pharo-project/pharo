private
evaluateBaselineMethodSection: methodSection
  | versionSpec |
  versionSpec := self project versionSpec.
  methodSection versionSpec: versionSpec.
  currentSection := methodSection.
  self with: versionSpec during: methodSection block.
  methodSection methodSections
    do: [ :ms | self evaluateBaselineMethodSection: ms ]