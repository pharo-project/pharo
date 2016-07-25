A NaturalLanguageTranslator is a dummy translator.

The localization framework is found in the gettext package usually 
overriding this class completely. 

As an alternative you can register a translator using
  
   NaturalLanguageTranslator current: myTranslator

If this is done the messages will be dispatched to it