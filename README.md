# eevo
Erlang Evolution (eevo) is an application to implement [evolutionary algorithms](https://en.wikipedia.org/wiki/Evolutionary_algorithm) in erlang.

# Installation
Create your own project with rebar3.
 ```sh
 $ rebar3 new app yourapp
 ```

Then in your project path find rebar.config file and add enn as dependency under the deps key:
```erlang
{deps, 
    [
        {enn, {git, "https://github.com/BorjaEst/eevo.git", {tag, "<version>"}}}
    ]}.
```

Then using compile command, rebar3 will fetch the defined dependencies and compile them as well for your application.
```sh
$ rebar3 compile
```


# Usage
Load the app using your prefered method. For example in the project folder executing  rebar3 shell:
```sh
$ rebar3 shell
===> Booted eevo
```


All user functions are defined inside the module [src/eevo](https://github.com/BorjaEst/eevo/blob/master/src/eevo.erl), however here is an example:






## Measure performance and resources
First of all I woudl initialize the observer, so you can see the loads of the 
system and the ETS tables:
```erl
1> observer:start().
ok
```
> Here you can find the application eevo with a supervisor 'eevo_sup'.

## Define your first agent
You can create an agent defining the following map with this 3 features:
```erl
#{
    function  => The_agent_function :: function(),
    arguments => [Argument :: term()],
    mutation  => The_mutation_function :: function() 
}
```
Where:

### function
What the agent will run, it is a call to apply(Function, Arguments) which should return one of the following:
* **{next, Fun, Arg         }** Indicates the next function with the arguments to run.
* **{next, Fun, Arg, Actions}** Indicates the next function with the arguments to run plus an action (score for example).
* **{stop,   Reason         }** This agent will end so a new one can be spawned. 
* **{stop,   Reason, Actions}** This agent will end so a new one can be spawned plus an action (score for example).t 

### arguments
Is a list of arguments which will be passed to 'function' and 'mutation'. 
> Note this arguments are the initial arguments to pass to function. The arguments returned by that call are *NOT* replacing this ones. 

### mutation
This function will be used to modify the arguments when creating a new agent mutating an old one. It is a call to apply(Mutation, Arguments) which must return a new list of arguments.


### Example
In this case, we can create a simple agent that will print his score and end. When mutating the score of the new agent will change.
```erl
2> Function = fun(Score) -> io:format("Hi, this is my score: ~p ~n", [Score]), {stop, normal, [{score,Score}]} end.
#Fun<erl_eval.44.97283095>
3> Arguments = [1.0].
[1.0]
4> Mutation = fun(Score) -> Score + rand:uniform(20) - 10 end.
#Fun<erl_eval.44.97283095>
5> MyAgent = eevo:agent(#{function=>Function, arguments=>Arguments, mutation=>Mutation}). 
{1,agent}
```



















## Test your agent
The best way to know everything went correctly is to try to run the agent suing `fun eevo:run_as/1`:
TBD
```erl

```


## Create and run a population
Once you have some agents you would like to evolve to find the optima, you can create and run a population defining the name, initial agents, size and stop rules.
In this case we will create a population *'test'* that will run 2 agents in parallel. The cycle will stop after 10 generations/mutations.
```
7> Stop_rule = fun(#{generation:=X})-> X>=10 end.
#Fun<erl_eval.44.97283095>
8> eevo:run(test,[MyAgent],2,Stop_rule).                                                   
Hi, this is my score: 1.0 

.....

```
TODO: TBD





