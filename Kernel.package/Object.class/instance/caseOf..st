casing-To be deprecated
caseOf: aBlockAssociationCollection
	"DO NOT USE THIS METHOD! It will be removed from Pharo."

	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, report an error."

	^ self caseOf: aBlockAssociationCollection otherwise: [self caseError]

"| z | z := {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"| z | z := {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}"