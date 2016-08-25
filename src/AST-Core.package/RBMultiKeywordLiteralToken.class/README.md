I am used for a multikeyword literal, found in a literal array.
Scanning a message send with a multikeyword selector produces not this token.
Instead, that token stream would contain multiple RBKeywordTokens. 
But if a multikeyword selector is within a literal array
#(printOn:indent: )
this single selector printOn:indent: is scanned as one RBMultiKeywordLiteralToken(#printOn:indent:).
