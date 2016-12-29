# tg-rss-bot

中文 Telegram RSS 机器人 [@TheBlackRSSBot](https://telegram.me/TheBlackRSSBot)

（ID 都被占用了，所以随便起的）

## 使用

    /rss   - 显示当前订阅的 RSS 列表，加 raw 参数显示链接
    /sub   - 订阅一个 RSS: /sub http://example.com/feed.xml
    /unsub - 退订一个 RSS: /unsub http://example.com/feed.xml

## 编译

**依赖：**
- JRE or JDK >= 1.6
- [Leiningen](https://leiningen.org/)

```
git clone https://github.com/iovxw/tg-rss-bot.git
cd tg-rss-bot
lein uberjar
```

编译好的文件在 `./target/uberjar/tg-rss-bot-0.5.8-standalone.jar`

## 运行

**依赖：**
- JRE or JDK >= 1.6

```
java -jar tg-rss-bot-0.5.8-standalone.jar [bot-token]
```

`bot-token` 的申请请参照 <https://core.telegram.org/bots#creating-a-new-bot>

## License

The MIT License (MIT)

Copyright (c) 2016 iovxw
