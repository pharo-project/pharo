private
formattedModificationTextFor:aUser atStamp:aTimeStampString
|stamp|
stamp:= DateAndTime fromString:aTimeStampString.
^ String streamContents:[ :s |
			s << 'Last Modification :' << Character cr.
			s << Character tab << 'date:' << Character tab << stamp asDate asString << Character space << stamp asTime asString << Character cr.
 			s << Character tab << 'by:' << Character tab << Character tab << aUser ]