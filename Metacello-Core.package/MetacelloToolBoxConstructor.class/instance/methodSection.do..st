private
methodSection: methodSection do: aBlock

	methodSection methodSections do: aBlock.
	methodSection methodSections do: [ :ms | self methodSection: ms do: aBlock ]