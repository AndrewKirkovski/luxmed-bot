package com.lbs.server

import akka.actor.ActorSystem
import com.lbs.api.json.model.{Event, TermExt}
import com.lbs.bot.Bot
import com.lbs.bot.telegram.TelegramBot
import com.lbs.server.conversation._
import com.lbs.server.lang.Localization
import com.lbs.server.repository.model.Monitoring
import com.lbs.server.service.{ApiService, DataService, MonitoringService}
import org.jasypt.util.text.{StrongTextEncryptor, TextEncryptor}
import org.springframework.beans.factory.annotation.{Autowired, Value}
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty
import org.springframework.context.annotation.{Bean, Configuration}

@Configuration
class BootConfig {
  @Value("${security.secret}")
  private var secret: String = _

  @Value("${telegram.token:}")
  private var telegramBotToken: String = _

  @Autowired
  private var apiService: ApiService = _

  @Autowired
  private var dataService: DataService = _

  @Autowired
  private var monitoringService: MonitoringService = _

  @Autowired
  private var localization: Localization = _

  @Bean
  def actorSystem: ActorSystem = ActorSystem()

  @Bean
  def textEncryptor: TextEncryptor = {
    val encryptor = new StrongTextEncryptor
    encryptor.setPassword(secret)
    encryptor
  }

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def authFactory: MessageSourceTo[Auth] = source =>
    new Auth(source, dataService, unauthorizedHelpFactory, loginFactory, chatFactory)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def loginFactory: MessageSourceWithOriginatorTo[Login] = (source, originator) =>
    new Login(source, bot, dataService, apiService, textEncryptor, localization, originator)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def bookFactory: UserIdTo[Book] = userId =>
    new Book(
      userId,
      bot,
      apiService,
      dataService,
      monitoringService,
      localization,
      datePickerFactory,
      timePickerFactory,
      staticDataFactory,
      termsPagerFactory
    )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def bookWithTemplateFactory: UserIdTo[BookWithTemplate] = userId =>
    new BookWithTemplate(
      userId,
      bot,
      apiService,
      dataService,
      monitoringService,
      localization,
      datePickerFactory,
      timePickerFactory,
      termsPagerFactory
    )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def unauthorizedHelpFactory: MessageSourceTo[UnauthorizedHelp] = source =>
    new UnauthorizedHelp(source, bot)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def helpFactory: UserIdTo[Help] = userId => new Help(userId, bot, localization)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def monitoringsFactory: UserIdTo[Monitorings] =
    userId => new Monitorings(userId, bot, monitoringService, localization, monitoringsPagerFactory)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def monitoringsHistoryFactory: UserIdTo[MonitoringsHistory] =
    userId =>
      new MonitoringsHistory(
        userId,
        bot,
        monitoringService,
        localization,
        monitoringsHistoryPagerFactory,
        bookWithTemplateFactory
      )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def historyFactory: UserIdTo[HistoryViewer] =
    userId => new HistoryViewer(userId, bot, apiService, localization, historyPagerFactory)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def reservedVisitsFactory: UserIdTo[ReservedVisitsViewer] =
    userId => new ReservedVisitsViewer(userId, bot, apiService, localization, reservedVisitsPagerFactory)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def settingsFactory: UserIdTo[Settings] =
    userId => new Settings(userId, bot, dataService, localization)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def accountFactory: UserIdTo[Account] =
    userId => new Account(userId, bot, dataService, localization, router)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def chatFactory: UserIdTo[Chat] =
    userId =>
      new Chat(
        userId,
        dataService,
        monitoringService,
        bookFactory,
        helpFactory,
        monitoringsFactory,
        monitoringsHistoryFactory,
        historyFactory,
        reservedVisitsFactory,
        settingsFactory,
        accountFactory
      )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def datePickerFactory: UserIdWithOriginatorTo[DatePicker] = (userId, originator) =>
    new DatePicker(userId, bot, localization, originator)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def timePickerFactory: UserIdWithOriginatorTo[TimePicker] = (userId, originator) =>
    new TimePicker(userId, bot, localization, originator)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def staticDataFactory: UserIdWithOriginatorTo[StaticData] = (userId, originator) =>
    new StaticData(userId, bot, localization, originator)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def termsPagerFactory: UserIdWithOriginatorTo[Pager[TermExt]] = (userId, originator) =>
    new Pager[TermExt](
      userId,
      bot,
      (term: TermExt, page: Int, index: Int) => lang(userId).termEntry(term, page, index),
      (page: Int, pages: Int) => lang(userId).termsHeader(page, pages),
      Some("book"),
      localization,
      originator
    )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def reservedVisitsPagerFactory: UserIdWithOriginatorTo[Pager[Event]] = (userId, originator) =>
    new Pager[Event](
      userId,
      bot,
      (visit: Event, page: Int, index: Int) => lang(userId).reservedVisitEntry(visit, page, index),
      (page: Int, pages: Int) => lang(userId).reservedVisitsHeader(page, pages),
      Some("cancel"),
      localization,
      originator
    )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def historyPagerFactory: UserIdWithOriginatorTo[Pager[Event]] = (userId, originator) =>
    new Pager[Event](
      userId,
      bot,
      (event: Event, page: Int, index: Int) => lang(userId).historyEntry(event, page, index),
      (page: Int, pages: Int) => lang(userId).historyHeader(page, pages),
      None,
      localization,
      originator
    )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def monitoringsPagerFactory: UserIdWithOriginatorTo[Pager[Monitoring]] = (userId, originator) =>
    new Pager[Monitoring](
      userId,
      bot,
      (monitoring: Monitoring, page: Int, index: Int) => lang(userId).monitoringEntry(monitoring, page, index),
      (page: Int, pages: Int) => lang(userId).monitoringsHeader(page, pages),
      Some("cancel"),
      localization,
      originator
    )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def monitoringsHistoryPagerFactory: UserIdWithOriginatorTo[Pager[Monitoring]] = (userId, originator) =>
    new Pager[Monitoring](
      userId,
      bot,
      (monitoring: Monitoring, page: Int, index: Int) => lang(userId).monitoringHistoryEntry(monitoring, page, index),
      (page: Int, pages: Int) => lang(userId).monitoringsHistoryHeader(page, pages),
      Some("repeat"),
      localization,
      originator
    )(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def router: Router = new Router(authFactory)(actorSystem)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def telegram: TelegramBot = new TelegramBot(router ! _, telegramBotToken)

  @Bean
  @ConditionalOnProperty(name = Array("telegram.enabled"), havingValue = "true")
  def bot: Bot = new Bot(telegram)

  private def lang(userId: Login.UserId) = {
    localization.lang(userId.userId)
  }
}
