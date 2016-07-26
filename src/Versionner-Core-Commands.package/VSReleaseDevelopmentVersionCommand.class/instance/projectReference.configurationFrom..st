actions
projectReference: projectName configurationFrom: aMCVersion 
	| fullProjectSpec |

	fullProjectSpec := (aMCVersion spec packageNamed: projectName) projectReference.
	^ fullProjectSpec projectClassProject