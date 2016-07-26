utilities
addMethodDefinitionsForClass: className in: entries root: aClassEntry
	"Timestamp for methods is recreated from the package info."

	(entries
		select: [ :entry | entry fileName matchesRegex: aClassEntry fileName , '((class)|(instance))' , self slash , '[^' , self slash , ']+.st' ])
		do: [ :methodEntry | 
			| classIsMeta fileStream category source timestamp selector |
			classIsMeta := false.
			(methodEntry fileName beginsWith: aClassEntry fileName , 'class' , self slash)
				ifTrue: [ classIsMeta := true ].
			fileStream := self zipEntryContents: methodEntry.
			category := fileStream nextLine.
			source := fileStream upToEnd.
			selector := self methodSelectorFor: source.
			timestamp := self info author , ' ' , self info date mmddyyyy , ' ' , self info time print24.	" ]"
			definitions
				add:
					(MCMethodDefinition
						className: className
						classIsMeta: classIsMeta
						selector: selector
						category: category
						timeStamp: timestamp
						source: source) ]