accessing
directory: aDirectory
  super directory: aDirectory.
  repositoryProperties := nil.	"force properties to be reloaded from new location"
  self repositoryProperties	"NOW"