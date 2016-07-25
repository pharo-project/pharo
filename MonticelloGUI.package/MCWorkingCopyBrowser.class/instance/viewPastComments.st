actions
viewPastComments
	"Visualize past comments on a package."

       workingCopy ifNotNil:
               [(UIManager default
                       edit: (String streamContents:
                                       [:s|
                                       "Ignore the initial MCWorkingAncestry instance."
                                       workingCopy ancestry topologicalAncestors allButFirst
                                               do: [:versionInfo|
                                                       s nextPutAll: versionInfo summary]
                                               separatedBy: [s cr; cr"; next: 32 put: $-; cr; cr"]])
                       label: 'Version History: ',  workingCopy packageName) ]