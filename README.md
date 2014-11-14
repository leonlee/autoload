autoload
========

**Auto loading the beam to specify nodes on linux.**

1.**Prepare**:<br>
&emsp;$ git clone https://github.com/zhongwencool/autoload.git<br>
&emsp;$ cd autoload <br>
&emsp;$ rebar get-deps <br>
<br>
2. **Configure**:<br>
you only need configure the ./src/autoload.app.src file:<br>
&emsp;    ` {env, [{autoload_path,"./dev_patch/"},{node_cookie,[{test1@localhost,best},{test2@localhost,best}]},`<br>
&emsp;         `{log,"./log/console.log"}]}`<br>
2.1 make sure the autoload_path under the directory autoload/, put the beams here;<br>
2.2 make sure node_cookie :[{NodeName1,Cookie1},{nodeName2,Cookie2}] , the beams will be updated to those nodes;<br>
2.3 ./log/console.log  the log file,you could also find the last time log on ./log/console.log.bak <br>
<br>
3.**Install**:<br>
&emsp;$ rebar compile<br>
<br>
4.**start**:<br>
&emsp;$ sh ./start.sh<br>
<br>
5.**Check Log**:<br>
<br>
&emsp;$ tail ./log/console.log -f<br>

<p></p>

License
-------
In short, you can do anything you want with the code including using it as part
of you plan for world domination (if your successful can I have one of the nicer
countries please). No responsiblity it taken for the fitness of the any purpose,
etc, etc. The only thing I ask is that if you find a bug and fix send me the
patch. Likewise, feature suggestions and patches are welcome.

------
