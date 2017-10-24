tests - converting
testAsCamelCase

       self assert: 'A man, a plan, a canal, panama' asCamelCase = 'AMan,APlan,ACanal,Panama'.
       self assert: 'Here 123should % be 6 the name6 of the method' asCamelCase = 'Here123should%Be6TheName6OfTheMethod'.
 