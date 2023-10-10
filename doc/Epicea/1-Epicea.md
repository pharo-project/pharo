# Epicea: a Change Logger

Epicea is a tool that records code changes enriched with IDE events.

Epicea extends and could replace the traditional .changes logging mechanism, where the recorded code changes are incomplete and are not properly reified and thus it can be difficult to recover lost code after an image crash.

## Monitor & Ombu files

Epicea Monitor logs code changes and other IDE events. Such data is placed in .ombu files in a local directory. Each time a Pharo image opens, Epicea Monitor logs code changes into a new ombu file. This avoids the risk of loosing the data in an ombu file: two Pharo images will always write into different ombu files. 

Logging the code changes into different files can have a negative consequence: the changes of a single development session might be segmented into several log files. This will happen in the case where the developer had to close and open several times the Pharo image.

## Browsers

Epicea provides browsers to visualize and manipulate the code changes and IDE events that the Monitor logs.

### Session changes

It shows the code changes since Epicea started to log. This means it shows changes placed in potentially several ombu files.

### All changes

The windows show:

- all the logs in the local directory on the left panel, and
- the content of the selected log in the right panel.

This window visually connects the logs to help user to understand how the ombu files are related. Note that the code changes displayed on the right panel belong to only one log (different than in the Monitor UI, that might show changes in several logs).
