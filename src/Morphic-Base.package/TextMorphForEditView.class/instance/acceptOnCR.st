editing
acceptOnCR
	"Answer whether the receiver wants to accept when the Return key is hit"
	"This test is to make sure that existing instances that may be changed and get their acceptOnCR 
	do not blow up on our face. Now this test could be removed"

	^ acceptOnCR == true