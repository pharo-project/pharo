accessing
enableFilterWithAction: aBlock named: aString
	"Enables filtering and add an Action button. Not compatible with the search. If I am use, the function must be explicit."

	function := FTFilterFunctionWithAction table: self action: aBlock named: aString.
	self explicitFunction