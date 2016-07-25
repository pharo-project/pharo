private - actions
newUpdatesFromFile: aFileReference
	"
	A file should be of following format:

#Pharo2.0a
20000-Pha-Empty.cs
20001-Pha-Empty2.cs
20002-Pha-DelScriptLoader.cs
	
	 "
	^self newUpdatesFromString: aFileReference readStream contents
