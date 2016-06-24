ecrontab
========
 [Travis-CI](https://travis-ci.org/feng19/ecrontab)::
 [![Build Status](https://secure.travis-ci.org/feng19/ecrontab.svg)](https://travis-ci.org/feng19/ecrontab)

## Overview ##

crontab for erlang

## Feature ##

* like crontab in linux
* support year & second

## Usage ##

* start ecrontab:
```
ecrontab:start().
```

* add worker
```
ecrontab:add_worker(worker_name).
```

* add a undefined name task what can say hello every minute
```
ecrontab:add(worker_name, {'*','*','*','*','*'}, fun() -> io:format("every minute hello!~n") end).
```

* add a task what can say hello every second 5
```
ecrontab:add(undefined, {'*','*','*','*','*','*',5}, fun() -> io:format("every second 5 hello!~n") end).
```

* more use case can see
```
$cat test/ecrontab_parse_test.erl
```
