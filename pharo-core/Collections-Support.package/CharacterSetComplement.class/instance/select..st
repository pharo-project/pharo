collection ops
select: aBlock
	"Implementation note: selecting present is rejecting absent"
	
	^(absent reject: aBlock) complement