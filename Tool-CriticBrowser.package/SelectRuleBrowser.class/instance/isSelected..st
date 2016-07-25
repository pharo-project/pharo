private
isSelected: aRule
	"Check the rule identifier to not run twice the same rule"
	^ selectedRules anySatisfy: [ :rule | rule class uniqueIdentifierName = aRule class uniqueIdentifierName ]