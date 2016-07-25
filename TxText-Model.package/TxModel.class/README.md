I am the central class, representing the text.

Internally my instances are organized as a double-linked list of spans (TxBasicSpan subclasses). There are multiple kinds of spans, like: CharacterSpan and LineSeparator.

The list is terminated at both ends with special TxStartSpan and TxEndSpan instances (which means even empty text consists of at least two such spans). 

Spans carry the actual text content, like characters and attributes. 
The model is designed in a way that it should be fairly easy to extend it by introducing new kinds of spans later. 

I don't provide a direct interface for mutating/editing my data (and this is a very important point). Instead I am modified using position(s) (TxTextPosition) and/or selection(s) (TxInterval/TxSelection), providing a rich protocol for various operations over text. 

