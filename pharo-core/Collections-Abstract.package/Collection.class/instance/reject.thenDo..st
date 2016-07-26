enumerating
reject: rejectBlock thenDo: doBlock 
	"Utility method to improve readability."
	(self reject: rejectBlock) do: doBlock