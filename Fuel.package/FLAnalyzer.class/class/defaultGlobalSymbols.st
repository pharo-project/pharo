accessing
defaultGlobalSymbols

    ^ #(#Smalltalk #SourceFiles #Transcript #Undeclared #Display #TextConstants #ActiveWorld #ActiveHand #ActiveEvent #Sensor #Processor #SystemOrganization #World) select: [:each | self environment includesKey: each ]