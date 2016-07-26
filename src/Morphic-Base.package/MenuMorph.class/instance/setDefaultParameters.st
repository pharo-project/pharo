private
setDefaultParameters
	"change the receiver's appareance parameters"

	self
		color: self theme settings derivedMenuColor;
		borderWidth: self theme menuBorderWidth;
		borderColor: self theme menuBorderColor.
	 self theme settings flatMenu 
		ifFalse: [
			self borderStyle: BorderStyle thinGray.
			self
				hasDropShadow: true;
				shadowColor:  self theme menuShadowColor;
				shadowOffset: 1 @ 1].
	self layoutInset: 3.
	self cellInset: 0@1.
