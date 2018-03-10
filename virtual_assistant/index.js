const Telegraf = require("telegraf");
const session = require('telegraf/session');
const csv=require('csvtojson');
const commandParts = require('telegraf-command-parts')
const Token = "544047766:AAEnEHdBC3GOeKcjBpAiJREz136B6tT94e4";
const bot = new Telegraf(Token);
bot.use(commandParts());

const csvFilePath='data.csv';
var data = []; 
var particles = [];
var periods = [];

csv().fromFile(csvFilePath).on('json',(jsonObj, rowIndex)=>{
	data[jsonObj.Cuando] = jsonObj;
	periods.push(jsonObj.Cuando);
	if(rowIndex == 0){
		for(var idx in jsonObj) {
  			particles.push(idx);
  		}
  		particles.shift();
	}
}).on('done',(error)=>{
    console.log('Data loaded successfully');
});

bot.use(session());
bot.command("start", (ctx) => {
	var nombre = ctx.from.first_name;
	ctx.reply("Hola, " + nombre + "\n\n" + 
		"Soy el bot de ParticleAI. Puedes consultarme información sobre los distintos alérgenos que se pueden dar en varias ciudades de España.\n\n" + 
		"Las ciudades disponibles ahora mismo son: \n\n" +  
		"/Valencia\n\n");
});

bot.on('text', (ctx) => {
  if(ctx.state.command != undefined){
	var command = ctx.state.command["command"];
  	if(command == "Valencia"){
  		ctx.session.city = command;
  		ctx.reply("Los tipos de partículas registradas son: \n\n" + 
		  		  "/" + particles.join("\n/"));
  	}else if(particles.includes(command)){
		ctx.session.particle = command;
		ctx.reply("Los periodos disponibles son: \n\n" + 
				  "/" + periods.join("\n/"));
  	}else if(periods.includes(command)){
		ctx.session.period = command;
		var response = data[ctx.session.period][ctx.session.particle ];
		ctx.reply("Para las partículas de " + ctx.session.particle +
				  ", la previsión para " + ctx.session.period +
				  " en " + ctx.session.city + 
				  " es de: " + response + 
				  " ppm");
  	}
  }
});


bot.command('help', (ctx) => ctx.reply('Elige un tipo de polen.'));

bot.startPolling();