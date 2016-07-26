api-configuration
methodSectionsFrom: inputVersionStringOrSymbol do: methodSectionBlock
  | constructor version fromVersionString |
  constructor := self constructor.
  version := project version: inputVersionStringOrSymbol.
  fromVersionString := version versionString.	"resolve symbolic versions"
  constructor extractMethodSectionsFor: fromVersionString.
  constructor methodSectionsDo: methodSectionBlock