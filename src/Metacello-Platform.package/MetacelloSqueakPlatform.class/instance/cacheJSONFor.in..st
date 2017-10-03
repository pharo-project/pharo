private
cacheJSONFor: aWebResponse in: aDictionary

  self cacheMetadataFor: aWebResponse in: aDictionary.
  "Retain Json"
  ^ aDictionary  at: 'jsonObject' put: (MCFileTreeJsonParser parse: aWebResponse content).