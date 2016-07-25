accessing
contentsColor: aColor
	"when you set this contentsColor, it takes precedence over theme one. In certain case (such as for green as in SUnit) it is needed, normally you do not need it."
	contentsColor := aColor.
	contentsAttr color: aColor.
	