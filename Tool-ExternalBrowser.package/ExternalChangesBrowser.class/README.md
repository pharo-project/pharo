I'm a simple changes browser. When the user drop a .cs or .st files and select to view them, I display a sequenceable and simple list of changes and a way to file in the code. 

!! Examples

[[[
ExternalChangesBrowser new openWithSpec.

ExternalChangesBrowser openOn: Smalltalk changesFile fullName
]]]