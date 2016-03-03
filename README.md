# Interpreter
## an interpreter for a very simple Java/C-ish language

This is a project folder for PLC group project (EECS345, Spring '16).

### How to contribute

```
$ git pull git@github.com:danielyaa5/plc_project.git plc 	# clone the repository
$ vi ...	# make changes
$ git add <file> # add file
$ git commit -m 'commit message' # tell about the changes you made, small commits welcomed
$ git push -u origin master # push and setup the origin! Done!
```

#### Feedback from project 1
- `Mstate_update` is written poorly. You check if the first element matches your variable...and then continue to check if the variable is in the entire list? This is extremely non-recursive and needs to be fixed. Likely cause of some of your errors also.
- -1 `startstate` abstraction
- `Mvalue` is returning numbers and booleans? This is not functional...Also what is true? I hope this is meant to be 'true, because scheme won't know what to do with a true. 
- Avoid using `length` functions. Stick to `car`/`cdr` combinations to avoid unneeded iterations.
- In general code is not very functional/recursive. Some of these functions need to be rewritten and reorganized. Also try using the Mstate/Mvals defined in class.

## Credit
- Daniel Yakobian `djy18`
- Justin Wang `jsw104`
- Hun Jae Lee `hxl224`
