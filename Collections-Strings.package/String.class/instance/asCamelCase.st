converting
asCamelCase
       "Convert to CamelCase. Can be conveinient in conjunction with #asLegalSelector
     	 'A man, a plan, a canal, panama' asCamelCase.
       'A man, a plan, a canal, panama' asCamelCase asLegalSelector.
       'Here 123should % be 6 the name6 of the method' asCamelCase.
       'Here 123should % be 6 the name6 of the method' asCamelCase asLegalSelector."

      ^ self class streamContents: [:stream |
               self substrings do: [:sub |
                       stream nextPutAll: sub capitalized]]