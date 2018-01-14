package com.lightbend.akka.sample

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props }
import akka.routing.{ ActorRefRoutee, RoundRobinRoutingLogic, Router }
import scala.io.Source

case class Start(filename: String) {
}

object Compteur {
  def props: Props = Props[Compteur]
}

class Compteur extends Actor {
  var nbMots : Int = 0

  def receive = {
    case line: String =>
      nbMots = if((line == null || line.isEmpty))  0 else line.split("\\s").length
      sender ! nbMots
  }
}
object Master {
  def props: Props = Props[Master]
}
class Master extends Actor with ActorLogging {
  var total = 0
  var nbLignes = 0
  var nbLignesLues = 0

  var router = {
    val routees = Vector.fill(5) {
      val r = context.actorOf(Props[Compteur])
      context watch r
      ActorRefRoutee(r)
    }
    Router(RoundRobinRoutingLogic(), routees)
  }

  def receive = {
  case start : Start => {
    // Lire le fichier et pour chaque ligne envoyer un message au router avec la ligne lue
    val filename = start.filename
    var lines = Source.fromFile(filename).getLines
    nbLignes = Source.fromFile(filename).getLines.size;
    Source.fromFile(filename).getLines.foreach(line => {
      router.route(line, self)
    })

  }
    case c : Character => {
    // Afficher le resultat
      if (c == 'q') {
        log.info(s"Nb total de mot : ${total}")

      }
    }

    // Aggréger le nombre de mot d'une ligne au total de mot
    case nbMot : Int => {
      total += nbMot
      nbLignesLues += 1
      if (nbLignesLues == nbLignes) { // Dès que l'on a fini on s'envoi le signal de fin
        self ! 'q'
      }
    } 
  }
}

object AkkaQuickstart extends App {
  val system: ActorSystem = ActorSystem("compteur")

  val master: ActorRef = system.actorOf(Master.props, "master")

  master ! Start("file.txt")

  Thread.sleep(2000)
    
  system.terminate()
}
