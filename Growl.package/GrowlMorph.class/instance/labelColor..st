accessing
labelColor: aColor
	"when you set this labelColor, it takes precedence over theme one. In certain case (such as for green as in SUnit) it is needed, normally you do not need it."
	labelColor := aColor.
	labelAttr color: self labelColor.