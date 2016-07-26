formatting
addStyle: styleSymbol attribute: additionalAttribute from: start to: end
	self addAttributes: (self attributesFor: styleSymbol), {additionalAttribute} from: start to: end.