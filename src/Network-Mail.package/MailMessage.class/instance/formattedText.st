printing/formatting
formattedText
	"Answer a version of my text suitable for display.  This cleans up the header, decodes HTML, and things like that"

	
	^ self cleanedHeader asText, String cr , self bodyTextFormatted