running
setUp

	| pragma |

	pragma := (Object compiledMethodAt: #at:) penultimateLiteral at: #primitive:.
	
	atState := AdditionalMethodState selector: #at: with: pragma copy.
