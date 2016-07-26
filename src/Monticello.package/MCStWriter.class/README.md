I write Category/Classes/Methods/.... definitions into Monticello source.st file format found in .mcz packages.    

See also MCStReader

Example:

String streamContents: [:aStream| |writer|
    writer := MCStWriter on: aStream.
    writer writeDefinitions: {True asClassDefinition. False asClassDefinition}.
]