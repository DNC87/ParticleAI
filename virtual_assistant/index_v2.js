const Telegraf = require("telegraf");
const session = require('telegraf/session');
const csv=require('csvtojson');
const commandParts = require('telegraf-command-parts')
const Token = "544047766:AAEnEHdBC3GOeKcjBpAiJREz136B6tT94e4";
const bot = new Telegraf(Token);
bot.use(commandParts());

const csvFilePath='data.csv';
const csvPollenPath="polen.csv";
const csvPollutionPath="contaminacion.csv";
const csvPMPath ="particulas_suspension.csv";

var data = [];
var particles = [];
var periods = [];

var data_pollen = [];
var data_pollution = [];
var data_pm = [];  

var particles_pollen = [];
var particles_pollution = [];
var particles_pm = [];  

var types = ["polen", "contaminacion", "particulas_suspension"];

// Pollen CSV file
csv().fromFile(csvPollenPath).on('json',(jsonObj, rowIndex)=>{
	data_pollen[jsonObj.Cuando] = jsonObj;
	periods.push(jsonObj.Cuando);
	if(rowIndex == 0){
		for(var idx in jsonObj) {
  			particles_pollen.push(idx);
  		}
  		particles_pollen.shift();
	}
}).on('done',(error)=>{
    console.log('Data from '+ csvPollenPath +' loaded successfully');
});

// Pollution CSV file
csv().fromFile(csvPollutionPath).on('json',(jsonObj, rowIndex)=>{
	data_pollution[jsonObj.Cuando] = jsonObj;
	if(rowIndex == 0){
		for(var idx in jsonObj) {
  			particles_pollution.push(idx);
  		}
  		particles_pollution.shift();
	}
}).on('done',(error)=>{
    console.log('Data from '+ csvPollutionPath +' loaded successfully');
});

// PM CSV file
csv().fromFile(csvPMPath).on('json',(jsonObj, rowIndex)=>{
	data_pm[jsonObj.Cuando] = jsonObj;
	if(rowIndex == 0){
		for(var idx in jsonObj) {
  			particles_pm.push(idx);
  		}
  		particles_pm.shift();
	}
}).on('done',(error)=>{
    console.log('Data from '+ csvPMPath +' loaded successfully');
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
  		ctx.reply("Los tipos de contaminantes registrados son: \n\n" + 
		  		  "/polen" + "\n" + 
		  		  "/contaminacion" + "\n" + 
		  		  "/particulas_suspension" + "\n");
  	}else if(types.includes(command)){
		ctx.session.type = command;
		if(command=="polen"){particles = particles_pollen;} if(command=="contaminacion"){particles = particles_pollution;}else if (command=="particulas_suspension"){particles = particles_pm;} else{}
		ctx.reply("Los tipos de partículas registradas para el contaminante "+command+": \n\n" + 
		  		  "/" + particles.join("\n/"));
  	}else if(particles.includes(command)){
		ctx.session.particle = command;
		ctx.reply("Los periodos disponibles son: \n\n" + 
				  "/" + periods.join("\n/"));
  	}else if(periods.includes(command)){
		ctx.session.period = command;
		if(ctx.session.type=="polen"){data = data_pollen;} else if(ctx.session.type=="contaminacion"){data = data_pollution;} else if (ctx.session.type=="particulas_suspension"){data = data_pm;}else{}
		var response = data[ctx.session.period][ctx.session.particle];
		ctx.reply("Para las partículas de " + ctx.session.particle +
				  ", la media para " + ctx.session.period +
				  " en " + ctx.session.city + 
				  " es de: " + response );
	}
  }
});


bot.command('help', (ctx) => ctx.reply('Elige un tipo de polen.'));

bot.startPolling();