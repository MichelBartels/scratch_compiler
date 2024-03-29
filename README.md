This is a scratch interpreter that supports the following subset of Scratch:

<img width="333" alt="image" src="https://github.com/MichelBartels/scratch_compiler/assets/17650521/ebd35e1c-8885-4993-9b11-8eb09a335788">

There is currently no I/O. The only output is the values of the variables at the end of an execution. Only one entry point is supported. There is also no string support.
It takes as input the `project.json` of a scratch project. It can be obtained by unzipping `.sb3` file. The file path is currently hard coded as `example_code/ack/project.json`.
It is faster than Scratch VM at computing the Ackermann function. (Scratch VM takes around 2min. This interpreter takes 7.5s.)

The `project.json` is parsed by a mostly standard compliant JSON parser. However, I didn't implement escape codes because it wasn't necessary for correctly processing the code.

It can be executed using `dune exec scratch_compiler`.
