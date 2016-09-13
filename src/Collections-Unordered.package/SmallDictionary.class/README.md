SmallDictionary is a special dictionary optimized for small collections. In addition to the normal dictionary protocol, it also supports an #empty message which "empties" the collection but may hang on to the original elements (so it could collect garbage). Without #empty we would either need to create a new dictionary or explicitly remove everything from the dictionary. Both of these take more time and #empty.

Instance Variables:
keys <Array of: Object> array of keys (we don't use Associations for our key value pairs)
size <Integer> the size of the dictionary
values <Array of: Object> array of our values
