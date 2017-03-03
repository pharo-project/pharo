initailize-release
monadicBlock: aSingleArgBlock
	"Initialze the receiver's collation block to compare the results of evaluating aSingleArgBlock with each argument, and then collate the return values with the <=> method."

	collator := [:a :b | self collate: (aSingleArgBlock value: a) with: (aSingleArgBlock value: b)]